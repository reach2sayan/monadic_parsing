#include <algorithm>
#include <optional>
#include <string_view>
#include <type_traits>
#include <variant>

namespace parser {

//---------------------------------------------------------------------
// A parser for T is a callable thing that takes a "string" and returns
// (optionally) an T plus the "leftover string".

using parser_input_t = std::string_view;
template <typename T>
using parser_result_t = std::optional<std::pair<T, parser_input_t>>;

// various parser out types
template <typename P>
using parser_r_v_t = std::result_of_t<P(parser_input_t)>;
template <typename P>
using parser_pair_t = typename parser_r_v_t<P>::value_type;
template <typename P>
using parser_t = typename parser_pair_t<P>::first_type;

// fmap a function into a parser.
// F :: parser_t<P> -> a
template <typename F, typename P>
constexpr auto fmap(F&& f, P&& p) {
  using R = parser_result_t<std::result_of_t<F(parser_t<P>)>>;
  return
      [f = std::forward<F>(f), p = std::forward<P>(p)](parser_input_t i) -> R {
	const auto r = p(i);
	if (!r) return std::nullopt;
	return R(std::make_pair(f(r->first), r->second));
      };
}

// bind a function into a parser.
// F :: (parser_t<P>, parser_input_t) -> a
template <typename P, typename F>
constexpr auto bind(P&& p, F&& f) {
  using R = std::result_of_t<F(parser_t<P>, parser_input_t)>;
  return [=](parser_input_t i) -> R {
    const auto r = p(i);
    if (!r) return std::nullopt;
    return f(r->first, r->second);
  };
}

template <typename P, typename F>
constexpr auto operator>>=(P&& p, F&& f) {
  return bind(std::forward<P>(p), std::forward<F>(f));
}

// lift a value into a parser
template <typename T>
constexpr auto lift(T&& t) {
  return [t = std::forward<T>(t)](parser_input_t s) {
    return parser_result_t<T>(std::make_pair(std::move(t), s));
  };
}

template <typename T>
constexpr auto fail(T) {
  return [=](parser_input_t) -> parser_result_t<T> { return std::nullopt; };
}

template <typename T, typename ErrFn>
constexpr auto fail(T, ErrFn f) {
  return [=](parser_input_t t) -> parser_result_t<T> {
    f();
    return std::nullopt;
  };
}

// parser combinators

// alternation : try first parse, if it fails do second.
// must return the same type
template <
    typename P1, typename P2,
    typename = std::enable_if_t<std::is_same_v<parser_t<P1>, parser_t<P2>>>>
constexpr auto operator|(P1&& p1, P2&& p2) {
  return [=](parser_input_t i) {
    const auto r1 = p1(i);
    if (r1) return r1;
    return p2(i);
  };
}

// accumulation : run both parsers, in sequence, combine the outputs
// using the func. Both parsers must succeed
template <typename P1, typename P2, typename F,
	  typename R = std::result_of_t<F(parser_t<P1>, parser_t<P2>)>>
constexpr auto combine(P1&& p1, P2&& p2, F&& f) {
  return [=](parser_input_t i) -> parser_result_t<R> {
    const auto r1 = p1(i);
    if (!r1) return std::nullopt;
    const auto r2 = p2(r1->second);
    if (!r2) return std::nullopt;
    return parser_result_t<R>(
	std::make_pair(f(r1->first, r2->first), r2->second));
  };
}

// for convenience, overload < and > to mean sequencing parsers
// and returning the result of the chosen one
template <typename P1, typename P2, typename = parser_t<P1>,
	  typename = parser_t<P2>>
constexpr auto operator<(P1&& p1, P2&& p2) {
  return combine(std::forward<P1>(p1), std::forward<P2>(p2),
		 [](auto, const auto& r) { return r; });
}

template <typename P1, typename P2, typename = parser_t<P1>,
	  typename = parser_t<P2>>
constexpr auto operator>(P1&& p1, P2&& p2) {
  return combine(std::forward<P1>(p1), std::forward<P2>(p2),
		 [](const auto& r, auto) { return r; });
}

// apply ? (zero or one) of a parser
template <typename P>
constexpr auto zero_or_one(P&& p) {
  using R = parser_result_t<parser_input_t>;
  return [p = std::forward<P>(p)](parser_input_t s) -> R {
    const auto r = p(s);
    if (r) return r;
    return R(std::make_pair(parser_input_t(s.data(), 0), s));
  };
}

template <typename P, typename T, typename F>
constexpr std::pair<T, parser_input_t> accumulate_parse(parser_input_t s, P&& p,
							T init, F&& f) {
  while (!s.empty()) {
    const auto r = p(s);
    if (!r) return std::make_pair(init, s);
    init = f(init, r->first);
    s = r->second;
  }
  return std::make_pair(init, s);
}

template <typename P, typename T, typename F>
constexpr std::pair<T, parser_input_t> accumulate_n_parse(parser_input_t s,
							  P&& p, std::size_t n,
							  T init, F&& f) {
  while (n != 0) {
    const auto r = p(s);
    if (!r) return std::make_pair(init, s);
    init = f(init, r->first);
    s = r->second;
    --n;
  }
  return std::make_pair(init, s);
}

// apply * (zero or more) of a parser, accumulating the results according to a
// function F. F :: T -> (parse_t<P>, parse_input_t) -> T
template <typename P, typename T, typename F>
constexpr auto many(P&& p, T&& init, F&& f) {
  return [p = std::forward<P>(p), init = std::forward<T>(init),
	  f = std::forward<F>(f)](parser_input_t s) {
    return parser_result_t<T>(accumulate_parse(s, p, init, f));
  };
}

// apply + (one or more) of a parser, accumulating the results according to a
// function F. F :: T -> (parse_t<P>, parse_input_t) -> T
template <typename P, typename T, typename F>
constexpr auto many1(P&& p, T&& init, F&& f) {
  return [p = std::forward<P>(p), init = std::forward<T>(init),
	  f = std::forward<F>(f)](parser_input_t s) -> parser_result_t<T> {
    const auto r = p(s);
    if (!r) return std::nullopt;
    return parser_result_t<T>(
	accumulate_parse(r->second, p, f(init, r->first), f));
  };
}

// apply a parser exactly n times, accumulating the results according to a
// function F. F :: T -> (parse_t<P>, parse_input_t) -> T
template <typename P, typename T, typename F>
constexpr auto exactly_n(P&& p, std::size_t n, T&& init, F&& f) {
  return [p = std::forward<P>(p), n, init = std::forward<T>(init),
	  f = std::forward<F>(f)](parser_input_t s) {
    return parser_result_t<T>(accumulate_n_parse(s, p, n, init, f));
  };
}

// try to apply a parser, and if it fails, return a default
template <typename P, typename T = parser_t<P>>
constexpr auto option(T&& def, P&& p) {
  return
      [p = std::forward<P>(p), def = std::forward<T>(def)](parser_input_t s) {
	const auto r = p(s);
	if (r) return r;
	return parser_result_t<T>(std::make_pair(def, s));
      };
}

// apply many instances of a parser, with another parser interleaved.
// accumulate the results with the given function.
template <typename P1, typename P2, typename F0, typename F>
constexpr auto separated_by(P1&& p1, P2&& p2, F0&& init, F&& f) {
  using R = parser_result_t<std::result_of_t<F0()>>;
  return [p1 = std::forward<P1>(p1), p2 = std::forward<P2>(p2),
	  init = std::forward<F0>(init),
	  f = std::forward<F>(f)](parser_input_t s) -> R {
    const auto r = p1(s);
    if (!r) return R(std::make_pair(init(), s));
    const auto p = p2 < p1;
    return R(accumulate_parse(r->second, p, f(init(), r->first), f));
  };
}

// apply many instances of a parser, with another parser interleaved.
// accumulate the results with the given function.
template <typename P1, typename P2, typename T, typename F>
constexpr auto separated_by_val(P1&& p1, P2&& p2, T&& init, F&& f) {
  using R = parser_result_t<std::remove_reference_t<T>>;
  return [p1 = std::forward<P1>(p1), p2 = std::forward<P2>(p2),
	  init = std::forward<T>(init),
	  f = std::forward<F>(f)](parser_input_t s) -> R {
    const auto r = p1(s);
    if (!r) return R(std::make_pair(init, s));
    const auto p = p2 < p1;
    return R(accumulate_parse(r->second, p, f(init, r->first), f));
  };
}

// parse a given string
constexpr auto make_string_parser(std::string_view str) {
  return [=](parser_input_t s) -> parser_result_t<std::string_view> {
    const auto p =
	std::mismatch(str.cbegin(), str.cend(), s.cbegin(), s.cend());
    if (p.first == str.cend()) {
      const auto len =
	  static_cast<std::string_view::size_type>(s.cend() - p.second);
      return parser_result_t<std::string_view>(
	  std::make_pair(str, parser_input_t(p.second, len)));
    }
    return std::nullopt;
  };
}

constexpr auto make_char_parser(char c) {
  return [=](parser_input_t s) -> parser_result_t<char> {
    if (s.empty() || s[0] != c) return std::nullopt;
    return parser_result_t<char>(
	std::make_pair(c, parser_input_t(s.data() + 1, s.size() - 1)));
  };
}

// parse one of a set of chars
constexpr auto one_of(std::string_view chars) {
  return [=](parser_input_t s) -> parser_result_t<char> {
    if (s.empty()) return std::nullopt;
    // basic_string_view::find is supposed to be constexpr, but no...
    auto j = std::find(chars.cbegin(), chars.cend(), s[0]);
    if (j != chars.cend()) {
      return parser_result_t<char>(
	  std::make_pair(s[0], parser_input_t(s.data() + 1, s.size() - 1)));
    }
    return std::nullopt;
  };
}

// parse none of a set of chars
constexpr auto none_of(std::string_view chars) {
  return [=](parser_input_t s) -> parser_result_t<char> {
    if (s.empty()) return std::nullopt;
    // basic_string_view::find is supposed to be constexpr, but no...
    auto j = std::find(chars.cbegin(), chars.cend(), s[0]);
    if (j == chars.cend()) {
      return parser_result_t<char>(
	  std::make_pair(s[0], parser_input_t(s.data() + 1, s.size() - 1)));
    }
    return std::nullopt;
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
  return bind(one_of("123456789"sv), [](char x, parser_input_t rest) {
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
