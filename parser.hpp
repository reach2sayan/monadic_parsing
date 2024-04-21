#include <algorithm>
#include <optional>
#include <string_view>
#include <type_traits>
#include <variant>

namespace parser {

//---------------------------------------------------------------------
// A parser for T is a callable thing that takes a "string" and returns
// (optionally) an T plus the "leftover string".

struct Parser {
  using parser_input_t = std::string_view;

  template <typename T>
  using parser_result_t = std::optional<std::pair<T, parser_input_t>>;

  // various parser out types
  template <typename F>
  using parser_r_v_t = std::result_of_t<F(parser_input_t)>;

  template <typename P>
  using parser_pair_t = typename parser_r_v_t<P>::value_type;

  template <typename P>
  using parser_t = typename parser_pair_t<P>::first_type;

  // fmap a function into a parser.
  // F :: parser_t<P> -> a
  template <typename F, typename P>
  constexpr static auto fmap(F&& f, P&& p) {
    using R = parser_result_t<std::result_of_t<F(parser_t<P>)>>;
    return [f = std::forward<F>(f),
	    p = std::forward<P>(p)](parser_input_t i) -> R {
      const auto r = p(i);
      return r ? R{std::make_pair(f(r->first), r->second)} : std::nullopt;
    };
  }

  // bind a function into a parser.
  // F :: (parser_t<P>, parser_input_t) -> a
  template <typename P, typename F>
  constexpr static auto bind(P&& p, F&& f) {
    using R = std::result_of_t<F(parser_t<P>, parser_input_t)>;
    return [=](parser_input_t i) -> R {
      const auto r = p(i);
      return r ? f(r->first, r->second) : std::nullopt;
    };
  }

  // lift a value into a parser
  template <typename T>
  constexpr static auto lift(T&& t) {
    return [t = std::forward<T>(t)](parser_input_t s) {
      return parser_result_t<T>(std::make_pair(std::move(t), s));
    };
  }

  template <typename T>
  constexpr static auto fail(T) {
    return [=](parser_input_t) -> parser_result_t<T> { return std::nullopt; };
  }

  template <typename T, typename ErrFn>
  constexpr static auto fail(T, ErrFn f) {
    return [=](parser_input_t t) -> parser_result_t<T> {
      f();
      return std::nullopt;
    };
  }
  // accumulation : run both parsers, in sequence, combine the outputs
  // using the func. Both parsers must succeed
  template <typename P1, typename P2, typename F,
	    typename R = std::result_of_t<F(parser_t<P1>, parser_t<P2>)>>
  constexpr static auto combine(P1&& p1, P2&& p2, F&& f) {
    return [=](parser_input_t i) -> parser_result_t<R> {
      const auto r1 = p1(i);
      if (!r1) return std::nullopt;
      const auto r2 = p2(r1->second);
      if (!r2) return std::nullopt;
      return parser_result_t<R>(
	  std::make_pair(f(r1->first, r2->first), r2->second));
    };
  }

  template <typename P, typename T, typename F>
  constexpr static std::pair<T, parser_input_t> accumulate_parse(
      parser_input_t s, P&& p, T init, F&& f) {
    while (!s.empty()) {
      const auto r = p(s);
      if (!r) return std::make_pair(init, s);
      init = f(init, r->first);
      s = r->second;
    }
    return std::make_pair(init, s);
  }

  template <typename P, typename T, typename F>
  constexpr static std::pair<T, parser_input_t> accumulate_n_parse(
      parser_input_t s, P&& p, std::size_t n, T init, F&& f) {
    while (n != 0) {
      const auto r = p(s);
      if (!r) return std::make_pair(init, s);
      init = f(init, r->first);
      s = r->second;
      --n;
    }
    return std::make_pair(init, s);
  }
};

template <typename P, typename F>
constexpr auto operator>>=(P&& p, F&& f) {
  return Parser::bind(std::forward<P>(p), std::forward<F>(f));
}
template <typename P, typename F>
constexpr auto operator>>(F&& f, P&& p) {
  return Parser::fmap(std::forward<F>(f), std::forward<P>(p));
}

// parser combinators

// alternation : try first parse, if it fails do second.
// must return the same type
template <typename P1, typename P2,
	  typename = std::enable_if_t<
	      std::is_same_v<Parser::parser_t<P1>, Parser::parser_t<P2>>>>
constexpr auto operator|(P1&& p1, P2&& p2) {
  return [=](Parser::parser_input_t i) {
    const auto r1 = p1(i);
    return r1 ? r1 : p2(i);
  };
}

// for convenience, overload < and > to mean sequencing parsers
// and returning the result of the chosen one
template <typename P1, typename P2, typename = Parser::parser_t<P1>,
	  typename = Parser::parser_t<P2>>
constexpr auto operator<(P1&& p1, P2&& p2) {
  return Parser::combine(std::forward<P1>(p1), std::forward<P2>(p2),
			 [](auto, const auto& r) { return r; });
}

template <typename P1, typename P2, typename = Parser::parser_t<P1>,
	  typename = Parser::parser_t<P2>>
constexpr auto operator>(P1&& p1, P2&& p2) {
  return Parser::combine(std::forward<P1>(p1), std::forward<P2>(p2),
			 [](const auto& r, auto) { return r; });
}

// apply ? (zero or one) of a parser
template <typename P>
constexpr auto zero_or_one(P&& p) {
  using R = Parser::parser_result_t<Parser::parser_input_t>;
  return [p = std::forward<P>(p)](Parser::parser_input_t s) -> R {
    const auto r = p(s);
    return r ? r : R(std::make_pair(Parser::parser_input_t(s.data(), 0), s));
  };
}

// apply * (zero or more) of a parser, accumulating the results according to a
// function F. F :: T -> (parse_t<P>, parse_input_t) -> T
template <typename P, typename T, typename F>
constexpr auto many(P&& p, T&& init, F&& f) {
  return [p = std::forward<P>(p), init = std::forward<T>(init),
	  f = std::forward<F>(f)](Parser::parser_input_t s) {
    return Parser::parser_result_t<T>(Parser::accumulate_parse(s, p, init, f));
  };
}

// apply + (one or more) of a parser, accumulating the results according to a
// function F. F :: T -> (parse_t<P>, parse_input_t) -> T
template <typename P, typename T, typename F>
constexpr auto many1(P&& p, T&& init, F&& f) {
  return [p = std::forward<P>(p), init = std::forward<T>(init),
	  f = std::forward<F>(f)](
	     Parser::parser_input_t s) -> Parser::parser_result_t<T> {
    const auto r = p(s);
    return r ? Parser::parser_result_t<T>(
		   Parser::accumulate_parse(r->second, p, f(init, r->first), f))
	     : std::nullopt;
  };
}

// apply a parser exactly n times, accumulating the results according to a
// function F. F :: T -> (parse_t<P>, parse_input_t) -> T
template <typename P, typename T, typename F>
constexpr auto exactly_n(P&& p, std::size_t n, T&& init, F&& f) {
  return [p = std::forward<P>(p), n, init = std::forward<T>(init),
	  f = std::forward<F>(f)](Parser::parser_input_t s) {
    return Parser::parser_result_t<T>(
	Parser::accumulate_n_parse(s, p, n, init, f));
  };
}

// try to apply a parser, and if it fails, return a default
template <typename P, typename T = Parser::parser_t<P>>
constexpr auto option(T&& def, P&& p) {
  return [p = std::forward<P>(p),
	  def = std::forward<T>(def)](Parser::parser_input_t s) {
    const auto r = p(s);
    return r ? r : Parser::parser_result_t<T>(std::make_pair(def, s));
  };
}

// apply many instances of a parser, with another parser interleaved.
// accumulate the results with the given function.
template <typename P1, typename P2, typename F0, typename F>
constexpr auto separated_by(P1&& p1, P2&& p2, F0&& init, F&& f) {
  using R = Parser::parser_result_t<std::result_of_t<F0()>>;
  return [p1 = std::forward<P1>(p1), p2 = std::forward<P2>(p2),
	  init = std::forward<F0>(init),
	  f = std::forward<F>(f)](Parser::parser_input_t s) -> R {
    const auto r = p1(s);
    if (!r) return R(std::make_pair(init(), s));
    const auto p = p2 < p1;
    return R(Parser::accumulate_parse(r->second, p, f(init(), r->first), f));
  };
}

// apply many instances of a parser, with another parser interleaved.
// accumulate the results with the given function.
template <typename P1, typename P2, typename T, typename F>
constexpr auto separated_by_val(P1&& p1, P2&& p2, T&& init, F&& f) {
  using R = Parser::parser_result_t<std::remove_reference_t<T>>;
  return [p1 = std::forward<P1>(p1), p2 = std::forward<P2>(p2),
	  init = std::forward<T>(init),
	  f = std::forward<F>(f)](Parser::parser_input_t s) -> R {
    const auto r = p1(s);
    if (!r) return R(std::make_pair(init, s));
    const auto p = p2 < p1;
    return R(Parser::accumulate_parse(r->second, p, f(init, r->first), f));
  };
}

// parse a given string
constexpr auto make_string_parser(std::string_view str) {
  return [=](Parser::parser_input_t s)
	     -> Parser::parser_result_t<std::string_view> {
    const auto p =
	std::mismatch(str.cbegin(), str.cend(), s.cbegin(), s.cend());
    if (p.first == str.cend()) {
      const auto len =
	  static_cast<std::string_view::size_type>(s.cend() - p.second);
      return Parser::parser_result_t<std::string_view>(
	  std::make_pair(str, Parser::parser_input_t(p.second, len)));
    }
    return std::nullopt;
  };
}

// parse one of a set of chars
constexpr auto one_of(std::string_view chars) {
  return [=](Parser::parser_input_t s) -> Parser::parser_result_t<char> {
    if (s.empty()) return std::nullopt;
    auto j = std::find(chars.cbegin(), chars.cend(), s[0]);
    if (j != chars.cend()) {
      return Parser::parser_result_t<char>(std::make_pair(
	  s[0], Parser::parser_input_t(s.data() + 1, s.size() - 1)));
    }
    return std::nullopt;
  };
}

// parse none of a set of chars
constexpr auto none_of(std::string_view chars) {
  return [=](Parser::parser_input_t s) -> Parser::parser_result_t<char> {
    if (s.empty()) return std::nullopt;
    // basic_string_view::find is supposed to be constexpr, but no...
    auto j = std::find(chars.cbegin(), chars.cend(), s[0]);
    if (j == chars.cend()) {
      return Parser::parser_result_t<char>(std::make_pair(
	  s[0], Parser::parser_input_t(s.data() + 1, s.size() - 1)));
    }
    return std::nullopt;
  };
}

// parse a given char
constexpr auto make_char_parser(char c) {
  return [=](Parser::parser_input_t s) -> Parser::parser_result_t<char> {
    return (s.empty() || s[0] != c)
	       ? std::nullopt
	       : Parser::parser_result_t<char>(std::make_pair(
		     c, Parser::parser_input_t(s.data() + 1, s.size() - 1)));
  };
}

// parse an int (may begin with 0)
constexpr auto int0_parser() {
  using namespace std::literals;
  return many1(one_of("0123456789"sv), 0,
	       [](int acc, char c) { return (acc * 10) + (c - '0'); });
}

// parse an int (may not begin with 0)
constexpr auto int1_parser() {
  using namespace std::literals;
  return Parser::bind(one_of("123456789"sv), [](char x,
						Parser::parser_input_t rest) {
    return many(one_of("0123456789"sv), static_cast<int>(x - '0'),
		[](int acc, char c) { return (acc * 10) + (c - '0'); })(rest);
  });
}

// a parser for skipping whitespace
constexpr auto skip_whitespace() {
  constexpr auto ws_parser = make_char_parser(' ') | make_char_parser('\t') |
			     make_char_parser('\n') | make_char_parser('\r');
  return many(ws_parser, std::monostate{}, [](auto m, auto) { return m; });
}

}  // namespace parser

constexpr auto bool_parser() {
  using namespace std::literals;
  return Parser::fmap([](std::string_view) { return true; },
		      make_string_parser("true"sv)) |
	 Parser::fmap([](std::string_view) { return false; },
		      make_string_parser("false"sv));
}

// parse a JSON null value
constexpr auto null_parser() {
  using namespace std::literals;
  return Parser::fmap([](std::string_view) { return std::monostate{}; },
		      make_string_parser("null"sv));
}

constexpr auto number_parser() {
  constexpr auto neg_parser = option('+', make_char_parser('-'));
  constexpr auto integral_parser = Parser::combine(
      neg_parser,
      Parser::fmap([](char) { return 0; }, make_char_parser('0')) |
	  int1_parser(),
      [](char sign, int i) { return sign == '+' ? i : -i; });

  constexpr auto frac_parser = make_char_parser('.') < int0_parser();

  constexpr auto mantissa_parser = Parser::combine(
      integral_parser, option(0, frac_parser), [](int i, int f) -> double {
	double d = 0;
	while (f > 0) {
	  d += f % 10;
	  d /= 10;
	  f /= 10;
	}
	return i + d;
      });

  constexpr auto e_parser = make_char_parser('e') | make_char_parser('E');
  constexpr auto sign_parser = make_char_parser('+') | neg_parser;
  constexpr auto exponent_parser =
      Parser::bind(e_parser < sign_parser, [](const char sign, const auto& sv) {
	return Parser::fmap([sign](int j) { return sign == '+' ? j : -j; },
			    int0_parser())(sv);
      });

  return Parser::combine(mantissa_parser, option(0, exponent_parser),
			 [](double mantissa, int exp) {
			   if (exp > 0) {
			     while (exp--) {
			       mantissa *= 10;
			     }
			   } else {
			     while (exp++) {
			       mantissa /= 10;
			     }
			   }
			   return mantissa;
			 });
}

constexpr auto convert_escaped_char(char c) {
  switch (c) {
    case 'b':
      return '\b';
    case 'f':
      return '\f';
    case 'n':
      return '\n';
    case 'r':
      return '\r';
    case 't':
      return '\t';
    default:
      return c;
  }
}

// convert a unicode code point to utf-8
constexpr auto to_utf8(uint32_t hexcode) {
  std::basic_string<char> s;
  if (hexcode <= 0x7f) {
    s.push_back(static_cast<char>(hexcode));
  } else if (hexcode <= 0x7ff) {
    s.push_back(static_cast<char>(0xC0 | (hexcode >> 6)));
    s.push_back(static_cast<char>(0x80 | (hexcode & 0x3f)));
  } else if (hexcode <= 0xffff) {
    s.push_back(static_cast<char>(0xE0 | (hexcode >> 12)));
    s.push_back(static_cast<char>(0x80 | ((hexcode >> 6) & 0x3f)));
    s.push_back(static_cast<char>(0x80 | (hexcode & 0x3f)));
  } else if (hexcode <= 0x10ffff) {
    s.push_back(static_cast<char>(0xF0 | (hexcode >> 18)));
    s.push_back(static_cast<char>(0x80 | ((hexcode >> 12) & 0x3f)));
    s.push_back(static_cast<char>(0x80 | ((hexcode >> 6) & 0x3f)));
    s.push_back(static_cast<char>(0x80 | (hexcode & 0x3f)));
  }
  return s;
}

constexpr auto to_hex(char c) {
  if (c >= '0' && c <= '9') return static_cast<uint16_t>(c - '0');
  if (c >= 'a' && c <= 'f') return static_cast<uint16_t>(c - 'a' + 10);
  return static_cast<uint16_t>(c - 'A' + 10);
}

constexpr auto unicode_point_parser() {
  using namespace std::literals;
  constexpr auto p = make_char_parser('\\') < make_char_parser('u') <
		     exactly_n(
			 one_of("0123456789abcdefABCDEF"sv), 4, 0u,
			 [](uint16_t hexcode, char c) -> uint16_t {
			   return (hexcode << 4) + to_hex(c);
			 });
  return Parser::fmap(to_utf8, p);
}

constexpr auto string_char_parser() {
  using namespace std::literals;
  constexpr auto slash_parser = make_char_parser('\\');
  constexpr auto special_char_parser =
      make_char_parser('"') | make_char_parser('\\') | make_char_parser('/') |
      make_char_parser('b') | make_char_parser('f') | make_char_parser('n') |
      make_char_parser('r') | make_char_parser('t');
  constexpr auto escaped_char_parser =
      Parser::fmap(convert_escaped_char, slash_parser < special_char_parser);
  constexpr auto p = escaped_char_parser | none_of("\\\""sv);

  return Parser::fmap(
	     [](auto c) {
	       std::basic_string<char> s;
	       s.push_back(c);
	       return s;
	     },
	     p) |
	 unicode_point_parser();
}

