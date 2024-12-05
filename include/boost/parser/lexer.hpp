// Copyright (C) 2024 T. Zachary Laine
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
#ifndef BOOST_PARSER_LEXER_HPP
#define BOOST_PARSER_LEXER_HPP

#include <boost/parser/config.hpp>

#if !BOOST_PARSER_USE_CONCEPTS || !__has_include(<ctre-unicode.hpp>)
#error                                                                         \
    "In order to work, the Boost.Parser lexer requires C++20 and CTRE's ctre-unicode.hpp single-header file in the #include path.  CTRE can be found at https://github.com/hanickadot/compile-time-regular-expressions .  The required header is at https://raw.githubusercontent.com/hanickadot/compile-time-regular-expressions/refs/heads/main/single-header/ctre-unicode.hpp ."
#endif

#if defined(BOOST_PARSER_PARSER_HPP)
#error "lexer.hpp must be included before parser.hpp."
#endif

#include <boost/parser/lexer_fwd.hpp>
#include <boost/parser/parser_fwd.hpp>
#include <boost/parser/concepts.hpp>
#include <boost/parser/detail/debug_assert.hpp>
#include <boost/parser/detail/hl.hpp>
#include <boost/parser/detail/numeric.hpp>

#include <ctre-unicode.hpp>

#include <climits>
#if defined(BOOST_PARSER_TESTING)
#include <iostream>
#endif
#include <sstream>
#include <string_view>
#include <type_traits>
#include <vector>


#if defined(BOOST_PARSER_TESTING)
template<typename T, size_t N>
std::ostream & operator<<(std::ostream & os, std::array<T, N> const & arr)
{
    bool first = true;
    for (auto x : arr) {
        if (!first)
            os << ", ";
        os << x;
        first = false;
    }
    return os;
}
#endif

namespace boost { namespace parser {

    namespace detail {
        enum class token_kind { string_view, long_long, long_double };

        enum class token_parsed_type {
            ws,

            character,

            string_view,

            bool_,

            char_,
            signed_char,
            unsigned_char,
            short_,
            unsigned_short,
            int_,
            unsigned_int,
            long_,
            unsigned_long,
            long_long,
            unsigned_long_long,
            wchar_t_,
            char8_t_,
            char16_t_,
            char32_t_,

            float_,
            double_,
            long_double
        };

#if defined(BOOST_PARSER_TESTING)
        inline std::ostream &
        operator<<(std::ostream & os, token_parsed_type type)
        {
#define BOOST_PARSER_CASE(name)                                                \
    case token_parsed_type::name: os << #name; break
            switch (type) {
                BOOST_PARSER_CASE(ws);
                BOOST_PARSER_CASE(character);
                BOOST_PARSER_CASE(string_view);
                BOOST_PARSER_CASE(bool_);
                BOOST_PARSER_CASE(char_);
                BOOST_PARSER_CASE(signed_char);
                BOOST_PARSER_CASE(unsigned_char);
                BOOST_PARSER_CASE(short_);
                BOOST_PARSER_CASE(unsigned_short);
                BOOST_PARSER_CASE(int_);
                BOOST_PARSER_CASE(unsigned_int);
                BOOST_PARSER_CASE(long_);
                BOOST_PARSER_CASE(unsigned_long);
                BOOST_PARSER_CASE(long_long);
                BOOST_PARSER_CASE(unsigned_long_long);
                BOOST_PARSER_CASE(wchar_t_);
                BOOST_PARSER_CASE(char8_t_);
                BOOST_PARSER_CASE(char16_t_);
                BOOST_PARSER_CASE(char32_t_);
                BOOST_PARSER_CASE(float_);
                BOOST_PARSER_CASE(double_);
                BOOST_PARSER_CASE(long_double);
#undef BOOST_PARSER_CASE
            }
            return os;
        }
#endif

        struct parse_spec
        {
            token_parsed_type type = token_parsed_type::ws;
            int radix = 10;
        };

#if defined(BOOST_PARSER_TESTING)
        inline std::ostream & operator<<(std::ostream & os, parse_spec spec)
        {
            return os << '{' << spec.type << ", " << spec.radix << '}';
        }
#endif

        template<auto... Values>
        struct nttp_array
        {
            template<auto... OtherValues>
            static constexpr nttp_array<Values..., OtherValues...> append()
            {
                return {};
            }

            static constexpr auto as_array() { return std::array{Values...}; }
        };
    }

    /** A convenience constant for specifying the empty string as the
        whitespace template parameter to `boost::parser::lexer`. */
    inline constexpr ctll::fixed_string no_ws = "";

    /** A token produced by the lexer during token parsing. */
    template<typename CharType>
    struct token
    {
        using char_type = CharType;
        using string_view = std::basic_string_view<CharType>;
        using position_type = BOOST_PARSER_TOKEN_POSITION_TYPE;

        constexpr token() :
            value_(0ll),
            underlying_position_(),
            id_(),
            kind_(detail::token_kind::string_view)
        {}
        constexpr token(
            int id, position_type underlying_position, string_view value) :
            value_(0ll),
            underlying_position_(),
            id_(id),
            kind_(detail::token_kind::string_view)
        {
            value_.sv_ = value;
        }
        constexpr token(
            int id, position_type underlying_position, long long value) :
            value_(0ll),
            underlying_position_(underlying_position),
            id_(id),
            kind_(detail::token_kind::long_long)
        {
            value_.ll_ = value;
        }
        constexpr token(
            int id, position_type underlying_position, long double value) :
            value_(0ll),
            underlying_position_(underlying_position),
            id_(id),
            kind_(detail::token_kind::long_double)
        {
            value_.d_ = value;
        }

        constexpr int id() const { return id_; }
        constexpr position_type underlying_position() const
        {
            return underlying_position_;
        }

        constexpr bool has_string_view() const
        {
            return kind_ == detail::token_kind::string_view;
        }
        constexpr string_view get_string_view() const
        {
            BOOST_PARSER_DEBUG_ASSERT(kind_ == detail::token_kind::string_view);
            return value_.sv_;
        }

        constexpr bool has_long_long() const
        {
            return kind_ == detail::token_kind::long_long;
        }
        constexpr long long get_long_long() const
        {
            BOOST_PARSER_DEBUG_ASSERT(kind_ == detail::token_kind::long_long);
            return value_.ll_;
        }

        constexpr bool has_long_double() const
        {
            return kind_ == detail::token_kind::long_double;
        }
        constexpr long double get_long_double() const
        {
            BOOST_PARSER_DEBUG_ASSERT(kind_ == detail::token_kind::long_double);
            return value_.d_;
        }

        constexpr bool operator==(token const & rhs) const
        {
            switch (rhs.kind_) {
            case detail::token_kind::string_view:
                return get_string_view() == rhs.get_string_view();
            case detail::token_kind::long_long:
                return get_long_long() == rhs.get_long_long();
            case detail::token_kind::long_double:
                return get_long_double() == rhs.get_long_double();
            default: BOOST_PARSER_DEBUG_ASSERT(!"Error: invalid token kind.");
#if defined(__cpp_lib_unreachable)
                std::unreachable();
#endif
                return false;
            }
        }

    private:
        union value
        {
            long long ll_;
            long double d_;
            string_view sv_;
        } value_;
        position_type underlying_position_ = 0;
        int id_ : 24;
        detail::token_kind kind_ : 8;
    };

#if defined(BOOST_PARSER_TESTING)
    template<typename CharType>
    std::ostream & operator<<(std::ostream & os, token<CharType> const & token)
    {
        os << "[value: ";
        if (token.has_string_view()) {
            os << '"' << token.get_string_view() << '"';
        } else if (token.has_long_long()) {
            if (token.id() == character_id) {
                os << "'" << (char)token.get_long_long() << "'";
            } else {
                os << token.get_long_long();
            }
        } else if (token.has_long_double()) {
            os << token.get_long_double();
        } else {
            os << "{no-value}";
        }
        os << ", id: ";
        if (token.id() == ws_id) {
            os << "{whitespace}";
        } else if (token.id() == character_id) {
            os << "{character}";
        } else {
            os << token.id();
        }
        os << "]";
        return os;
    }
#endif

    namespace detail {
        template<typename TokenSpec>
        constexpr parse_spec parse_spec_for()
        {
            using value_t = typename TokenSpec::value_type;
            constexpr auto base = TokenSpec::base;
            if constexpr (TokenSpec::is_character_token) {
                return parse_spec{token_parsed_type::character, base};
            } else if constexpr (std::is_same_v<value_t, string_view_tag>) {
                return parse_spec{token_parsed_type::string_view, base};
            } else if constexpr (std::is_same_v<value_t, bool>) {
                return parse_spec{token_parsed_type::bool_, base};
            } else if constexpr (std::is_same_v<value_t, char>) {
                return parse_spec{token_parsed_type::char_, base};
            } else if constexpr (std::is_same_v<value_t, signed char>) {
                return parse_spec{token_parsed_type::signed_char, base};
            } else if constexpr (std::is_same_v<value_t, unsigned char>) {
                return parse_spec{token_parsed_type::unsigned_char, base};
            } else if constexpr (std::is_same_v<value_t, short>) {
                return parse_spec{token_parsed_type::short_, base};
            } else if constexpr (std::is_same_v<value_t, unsigned short>) {
                return parse_spec{token_parsed_type::unsigned_short, base};
            } else if constexpr (std::is_same_v<value_t, int>) {
                return parse_spec{token_parsed_type::int_, base};
            } else if constexpr (std::is_same_v<value_t, unsigned int>) {
                return parse_spec{token_parsed_type::unsigned_int, base};
            } else if constexpr (std::is_same_v<value_t, long>) {
                return parse_spec{token_parsed_type::long_, base};
            } else if constexpr (std::is_same_v<value_t, unsigned long>) {
                return parse_spec{token_parsed_type::unsigned_long, base};
            } else if constexpr (std::is_same_v<value_t, long long>) {
                return parse_spec{token_parsed_type::long_long, base};
            } else if constexpr (std::is_same_v<value_t, unsigned long long>) {
                return parse_spec{token_parsed_type::unsigned_long_long, base};
            } else if constexpr (std::is_same_v<value_t, wchar_t>) {
                return parse_spec{token_parsed_type::wchar_t_, base};
            } else if constexpr (std::is_same_v<value_t, char8_t>) {
                return parse_spec{token_parsed_type::char8_t_, base};
            } else if constexpr (std::is_same_v<value_t, char16_t>) {
                return parse_spec{token_parsed_type::char16_t_, base};
            } else if constexpr (std::is_same_v<value_t, char32_t>) {
                return parse_spec{token_parsed_type::char32_t_, base};
            } else if constexpr (std::is_same_v<value_t, float>) {
                return parse_spec{token_parsed_type::float_, base};
            } else if constexpr (std::is_same_v<value_t, double>) {
                return parse_spec{token_parsed_type::double_, base};
            } else if constexpr (std::is_same_v<value_t, long double>) {
                return parse_spec{token_parsed_type::long_double, base};
            } else {
                static_assert(
                    !std::is_same_v<TokenSpec, TokenSpec>,
                    "The only valid types for the 'Value' template parameter "
                    "to 'token_spec' are 'string_view_tag', integral types, "
                    "and floating-point types.");
            }
#if defined(__cpp_lib_unreachable)
            std::unreachable();
#endif
            return parse_spec{token_parsed_type::string_view, base};
        }

        template<char Ch, auto... Chs>
        struct token_chars_spec
        {
            static_assert(
                (unsigned char)Ch < 128u &&
                    ((unsigned char)(Chs < 128u) && ... && true),
                "All non-type template parameters given to token_chars_spec "
                "must be <= 127.");
        };

        constexpr bool is_pcre_metacharacter(unsigned char c)
        {
            constexpr unsigned char chars[] = {
                '$',
                '(',
                ')',
                '*',
                '+',
                '.',
                '?',
                '[',
                '\\',
                '^',
                '{',
                '|',
                '}'};
            auto const it = std::ranges::lower_bound(chars, c);
            return it != std::end(chars) && *it == c;
        }

        template<typename T>
        concept character_type =
            std::same_as<T, char> || std::same_as<T, char8_t>;

        template<auto Regex>
        consteval auto wrap_and_escape()
        {
            using regex_t = decltype(Regex);
            if constexpr (character_type<regex_t>) {
                if constexpr (detail::is_pcre_metacharacter(Regex)) {
                    char str[] = {'(', '\\', Regex, ')', 0};
                    return ctll::fixed_string{str};
                } else {
                    char str[] = {'(', Regex, ')', 0};
                    return ctll::fixed_string{str};
                }
            } else {
                char str[Regex.size() + 3] = {0};
                str[0] = '(';
                std::ranges::copy(Regex, str + 1);
                str[Regex.size() + 1] = ')';
                str[Regex.size() + 2] = 0;
                return ctll::fixed_string{str};
            }
        }

        template<auto Curr, auto Head, auto... Tail>
        consteval auto wrap_escape_concat()
        {
            // TODO: Try to find a way to eliminate the duplicate calls to
            // wrap_and_escape.

            char
                str[Curr.size() + detail::wrap_and_escape<Head>().size() +
                    (Curr.size() == 0u ? 0 : 1) +
                    (detail::wrap_and_escape<Tail>().size() + ... + 0) +
                    sizeof...(Tail) + 1] = {0};
            auto it = std::ranges::copy(Curr, str).out;
            if constexpr (Curr.size() != 0u) {
                *it++ = '|';
            }
            it = std::ranges::copy(detail::wrap_and_escape<Head>(), it).out;
            [[maybe_unused]] int const dummy =
                ((*it++ = '|',
                  it = std::ranges::copy(detail::wrap_and_escape<Tail>(), it)
                           .out),
                 ...,
                 0);
            *it++ = 0;
            return ctll::fixed_string{str};
        }
    }

    /** Represents the compile time parameters for matching a single token
        during token parsing, and for producing a `std::basic_string_view` or
        number from the matched characters.  Don't use this directly; use
        `boost::parser::token_spec` instead. */
    template<ctll::fixed_string Regex, auto ID, typename ValueType, int Base>
    struct token_spec_t
    {
        using id_type = decltype(ID);
        using value_type = ValueType;

        static_assert(
            0 <= (int)ID, "Token IDs must be integral values or enums >=0.");

        static constexpr ctll::fixed_string regex = Regex;
        static constexpr id_type id = ID;
        static constexpr int base = Base < 0 ? 10 : Base;
        static constexpr bool is_character_token = Base < 0;
    };

    /** Specifies one or more single-character tokens.  Each character must be
        in the ASCII range (< 128), and must be of type `char`.  If you want
        to specify tokens of longer than 1 character, use
        `boost::parser::token_spec`. */
    template<char Ch, auto... Chs>
        requires(std::same_as<decltype(Chs), char> && ... && true)
    constexpr auto token_chars = detail::token_chars_spec<Ch, Chs...>{};

    /** The type used to represent the lexer used to tokenize input during
        token parsing.  Do not use this directly; use `boost::parser::lexer`
        instead. */
    template<
        typename CharType,
        typename ID,
        ctll::fixed_string WsStr = "\\s+",
        ctll::fixed_string RegexStr = "",
        detail::nttp_array IDs = detail::nttp_array<-1>{},
        detail::nttp_array Specs = detail::nttp_array<detail::parse_spec{}>{}>
    struct lexer_t
    {
        using id_type = ID;
        using token_type = token<CharType>;
        using string_view = std::basic_string_view<CharType>;

        static constexpr ctll::fixed_string ws_str = WsStr;
        static constexpr ctll::fixed_string regex_str = RegexStr;
        static constexpr bool has_ws = WsStr.size() != 0u;

        static constexpr size_t size() { return ids().size(); }
        static constexpr auto ids() { return IDs.as_array(); }
        static constexpr auto specs() { return Specs.as_array(); }

        // implementation in token_parser.hpp
        template<
            ctll::fixed_string RegexStr2,
            auto ID2,
            typename ValueType,
            int Base>
        constexpr auto
        operator|(parser_interface<token_parser<
                      token_spec_t<RegexStr2, ID2, ValueType, Base>,
                      detail::nope>> const &) const;

        template<auto Ch, auto... Chs>
        constexpr auto
        operator|(detail::token_chars_spec<Ch, Chs...> const & rhs) const
        {
            constexpr auto new_regex =
                detail::wrap_escape_concat<regex_str, Ch, Chs...>();
            constexpr auto new_ids =
                IDs.template append<(int)Ch, (int)Chs...>();
            constexpr auto new_specs = Specs.template append<
                detail::parse_spec_for<token_spec_t<"", (ID)Ch, int, -1>>(),
                detail::parse_spec_for<
                    token_spec_t<"", (ID)Chs, int, -1>>()...>();
            return lexer_t<
                CharType,
                ID,
                WsStr,
                new_regex,
                new_ids,
                new_specs>{};
        }

        template<parsable_range V>
        static constexpr auto regex_range(V & base)
        {
            if constexpr (has_ws) {
                return ctre::multiline_tokenize<
                    detail::wrap_escape_concat<regex_str, WsStr>()>(base);
            } else {
                return ctre::multiline_tokenize<regex_str>(base);
            }
        }
    };

    // Note that the initial nttp_array values are there because CTRE has an
    // implicit 0-group that we ignore, but we still need initial elements to
    // make all the indices line up later.

    /** A variable template used to generate a lexer for use in token parsing.
        The resulting lexer has no associated tokens.  Associate tokens with
        it by piping `boost::parser::token_spec`s and/or
        `boost::parser::token_chars`s after it. */
    template<
        typename CharType,
        typename ID,
        ctll::fixed_string WsStr = "\\s+",
        ctll::fixed_string RegexStr = "",
        detail::nttp_array IDs = detail::nttp_array<-1>{},
        detail::nttp_array Specs = detail::nttp_array<detail::parse_spec{}>{}>
    constexpr auto lexer = lexer_t<CharType, ID, WsStr, RegexStr, IDs, Specs>{};

    namespace detail {
        template<typename T>
        struct type_wrapper
        {
            using type = T;
        };

        template<parse_spec Spec, typename CharType, typename TokenIter>
        token<CharType> make_token(
            int id,
            std::basic_string_view<CharType> ctre_token,
            BOOST_PARSER_TOKEN_POSITION_TYPE underlying_position,
            TokenIter it)
        {
            auto f = ctre_token.data();
            auto const l = f + ctre_token.size();

            // radix==0 indicates a real number was parsed.
            auto report_error = [it](auto type, int radix, bool success) {
                if (!success) {
                    using unwrapped_type = typename decltype(type)::type;
                    std::ostringstream oss;
                    auto const bytes = sizeof(unwrapped_type);
                    oss << (bytes * CHAR_BIT) << "-bit";
                    if (!radix) {
                        oss << " floating-point number";
                    } else {
                        if (radix != 10)
                            oss << ", base-" << radix;
                        oss << (std::is_signed_v<unwrapped_type> ? " " : " un");
                        oss << "signed integer";
                    }
                    throw lex_error<TokenIter>(it, oss.str());
                }
            };

            switch (Spec.type) {
            case token_parsed_type::character:
                return {
                    character_id,
                    underlying_position,
                    (long long)ctre_token[0]};

            case token_parsed_type::string_view:
                return {id, underlying_position, ctre_token};

            case token_parsed_type::bool_:
                using namespace std::literals;
                if (std::ranges::equal(ctre_token, "true"sv)) {
                    return {id, underlying_position, 1ll};
                } else if (std::ranges::equal(ctre_token, "false"sv)) {
                    return {id, underlying_position, 0ll};
                } else {
                    throw lex_error<TokenIter>(it, "'true' or 'false'");
                }

            case token_parsed_type::char_: {
                char value;
                report_error(
                    type_wrapper<decltype(value)>{},
                    Spec.radix,
                    numeric::parse_int<true, Spec.radix, 1, -1>(f, l, value));
                return {id, underlying_position, (long long)value};
            }
            case token_parsed_type::signed_char: {
                signed char value;
                report_error(
                    type_wrapper<decltype(value)>{},
                    Spec.radix,
                    numeric::parse_int<true, Spec.radix, 1, -1>(f, l, value));
                return {id, underlying_position, (long long)value};
            }
            case token_parsed_type::unsigned_char: {
                unsigned char value;
                report_error(
                    type_wrapper<decltype(value)>{},
                    Spec.radix,
                    numeric::parse_int<false, Spec.radix, 1, -1>(f, l, value));
                return {id, underlying_position, (long long)value};
            }
            case token_parsed_type::short_: {
                short value;
                report_error(
                    type_wrapper<decltype(value)>{},
                    Spec.radix,
                    numeric::parse_int<true, Spec.radix, 1, -1>(f, l, value));
                return {id, underlying_position, (long long)value};
            }
            case token_parsed_type::unsigned_short: {
                unsigned short value;
                report_error(
                    type_wrapper<decltype(value)>{},
                    Spec.radix,
                    numeric::parse_int<false, Spec.radix, 1, -1>(f, l, value));
                return {id, underlying_position, (long long)value};
            }
            case token_parsed_type::int_: {
                int value;
                report_error(
                    type_wrapper<decltype(value)>{},
                    Spec.radix,
                    numeric::parse_int<true, Spec.radix, 1, -1>(f, l, value));
                return {id, underlying_position, (long long)value};
            }
            case token_parsed_type::unsigned_int: {
                unsigned int value;
                report_error(
                    type_wrapper<decltype(value)>{},
                    Spec.radix,
                    numeric::parse_int<false, Spec.radix, 1, -1>(f, l, value));
                return {id, underlying_position, (long long)value};
            }
            case token_parsed_type::long_: {
                long value;
                report_error(
                    type_wrapper<decltype(value)>{},
                    Spec.radix,
                    numeric::parse_int<true, Spec.radix, 1, -1>(f, l, value));
                return {id, underlying_position, (long long)value};
            }
            case token_parsed_type::unsigned_long: {
                unsigned long value;
                report_error(
                    type_wrapper<decltype(value)>{},
                    Spec.radix,
                    numeric::parse_int<false, Spec.radix, 1, -1>(f, l, value));
                return {id, underlying_position, (long long)value};
            }
            case token_parsed_type::long_long: {
                long long value;
                report_error(
                    type_wrapper<decltype(value)>{},
                    Spec.radix,
                    numeric::parse_int<true, Spec.radix, 1, -1>(f, l, value));
                return {id, underlying_position, (long long)value};
            }
            case token_parsed_type::unsigned_long_long: {
                unsigned long long value;
                report_error(
                    type_wrapper<decltype(value)>{},
                    Spec.radix,
                    numeric::parse_int<false, Spec.radix, 1, -1>(f, l, value));
                return {id, underlying_position, (long long)value};
            }
            case token_parsed_type::wchar_t_: {
                unsigned int value;
                report_error(
                    type_wrapper<wchar_t>{},
                    Spec.radix,
                    numeric::parse_int<false, Spec.radix, 1, -1>(f, l, value));
                return {id, underlying_position, (long long)value};
            }
            case token_parsed_type::char8_t_: {
                unsigned int value;
                report_error(
                    type_wrapper<char8_t>{},
                    Spec.radix,
                    numeric::parse_int<false, Spec.radix, 1, -1>(f, l, value));
                return {id, underlying_position, (long long)value};
            }
            case token_parsed_type::char16_t_: {
                unsigned int value;
                report_error(
                    type_wrapper<char16_t>{},
                    Spec.radix,
                    numeric::parse_int<false, Spec.radix, 1, -1>(f, l, value));
                return {id, underlying_position, (long long)value};
            }
            case token_parsed_type::char32_t_: {
                unsigned int value;
                report_error(
                    type_wrapper<char32_t>{},
                    Spec.radix,
                    numeric::parse_int<false, Spec.radix, 1, -1>(f, l, value));
                return {id, underlying_position, (long long)value};
            }

            case token_parsed_type::float_: {
                float value;
                report_error(
                    type_wrapper<decltype(value)>{},
                    0,
                    numeric::parse_real(f, l, value));
                return {id, underlying_position, (long double)value};
            }
            case token_parsed_type::double_: {
                double value;
                report_error(
                    type_wrapper<decltype(value)>{},
                    0,
                    numeric::parse_real(f, l, value));
                return {id, underlying_position, (long double)value};
            }
            case token_parsed_type::long_double: {
                long double value;
                report_error(
                    type_wrapper<decltype(value)>{},
                    0,
                    numeric::parse_real(f, l, value));
                return {id, underlying_position, value};
            }
            case token_parsed_type::ws:
            default:
#if defined(__cpp_lib_unreachable)
                std::unreachable();
#endif
                return {id, underlying_position, 0ll};
            }
        }
    }

    namespace detail {
        template<bool Const, typename T>
        using maybe_const = std::conditional_t<Const, T const, T>;
    }

    template<
        std::ranges::contiguous_range V,
        typename Lexer,
        typename TokenCache>
        requires std::ranges::view<V>
    struct tokens_view
        : public std::ranges::view_interface<tokens_view<V, Lexer, TokenCache>>
    {
#ifndef BOOST_PARSER_DOXYGEN
    private:
        template<bool>
        struct iterator;

        template<bool>
        struct sentinel;

        template<bool>
        friend struct iterator;

        using tokens_type = decltype(Lexer::regex_range(std::declval<V &>()));
        using const_tokens_type =
            decltype(Lexer::regex_range(std::declval<V const &>()));
#endif

    public:
        using token_type = typename Lexer::token_type;

        tokens_view()
            requires std::default_initializable<V>
            : base_(), lexer_(), tokens_(owned_cache_)
        {}
        constexpr explicit tokens_view(V base, Lexer lexer) :
            base_(std::move(base)),
            lexer_(std::move(lexer)),
            tokens_(owned_cache_)
        {
            latest_ = std::ranges::begin(base_);
        }
        constexpr explicit tokens_view(
            V base,
            Lexer lexer,
            std::reference_wrapper<TokenCache> external_cache) :
            base_(std::move(base)),
            lexer_(std::move(lexer)),
            tokens_(external_cache.get())
        {
            latest_ = std::ranges::begin(base_);
        }

        constexpr V base() const &
            requires std::copy_constructible<V>
        {
            return base_;
        }
        constexpr V base() && { return std::move(base_); }

        constexpr Lexer lexer() { return lexer_; }

        constexpr iterator<false> begin() { return iterator<false>(*this); }
        constexpr iterator<true> begin() const { return iterator<true>(*this); }

        constexpr sentinel<false> end() { return sentinel<false>(); }
        constexpr sentinel<true> end() const { return sentinel<true>(); }

#ifndef BOOST_PARSER_DOXYGEN
    private:
        // Called during parse after reaching an expectation point.
        template<bool Const>
        void clear_tokens_before(iterator<Const> it) const
        {
            size_t const tokens_pos = it.token_offset_ - base_token_offset_;
            tokens_.erase(tokens_.begin(), tokens_.begin() + tokens_pos);
            base_token_offset_ += tokens_pos;
        }

        // Called during parse when entering/leaving lexeme parsing.
        template<bool Const>
        void produce_ws_tokens(bool produce, iterator<Const> it) const
        {
            if (it == end())
                return;
            latest_ = std::ranges::begin(base_) + (*it).underlying_position();
            size_t const tokens_pos = it.token_offset_ - base_token_offset_;
            tokens_.erase(tokens_.begin() + tokens_pos, tokens_.end());
            produce_ws_tokens_ = produce;
        }

        template<
            typename ParserTuple,
            typename BacktrackingTuple,
            typename CombiningGroups>
        friend struct seq_parser;

        template<typename I, typename Context>
        friend struct detail::scoped_lexeme;

        // TODO: Document that the token cache will grow without bound if the
        // parser contains no sequence points.  Document this in the doc
        // section that talks about the importance of sequence points.

        V base_ = V();
        Lexer lexer_;
        mutable std::ranges::iterator_t<V> latest_;
        mutable TokenCache owned_cache_;
        TokenCache & tokens_;
        mutable size_t base_token_offset_ = 0;
        mutable bool produce_ws_tokens_ = false;

        template<bool Const>
        struct iterator
            : public detail::stl_interfaces::proxy_iterator_interface<
                  iterator<Const>,
                  std::forward_iterator_tag,
                  token_type>
        {
        private:
            using Parent = detail::maybe_const<Const, tokens_view>;
            using Base =
                std::conditional_t<Const, const_tokens_type, tokens_type>;
            using base_iterator_type = std::ranges::iterator_t<Base>;

            Parent * parent_ = nullptr;
            size_t token_offset_ = 0;

            friend tokens_view;
            friend tokens_view::sentinel<Const>;

            static constexpr size_t initial_tokens_cache_size = 64;

            iterator(Parent * parent, size_t token_offset) :
                parent_(parent), token_offset_(token_offset)
            {}
            iterator(Parent & parent) : parent_(&parent) { fill_cache(); }

            void fill_cache() const
            {
                using string_view = typename Lexer::string_view;

                size_t const new_size = parent_->tokens_.empty()
                                            ? initial_tokens_cache_size
                                            : parent_->tokens_.size() * 2;
                parent_->tokens_.reserve(new_size);

                auto r = std::ranges::subrange(
                    parent_->latest_, std::ranges::end(parent_->base_));
                auto ctre_range = Lexer::regex_range(r);
                auto ctre_first = ctre_range.begin();
                auto const ctre_last = ctre_range.end();

                size_t i = parent_->tokens_.size();
                for (; i < new_size && ctre_first != ctre_last; ++ctre_first) {
                    auto const parse_results = *ctre_first;

                    if constexpr (Lexer::has_ws) {
                        if (auto result =
                                parse_results.template get<Lexer::size()>()) {
                            string_view const sv = result;
                            if (parent_->produce_ws_tokens_) {
                                ++i;
                                parent_->tokens_.push_back(
                                    detail::make_token<detail::parse_spec{}>(
                                        ws_id,
                                        sv,
                                        (ctre_first.current -
                                         ctre_first.orig_begin) -
                                            sv.size(),
                                        iterator(parent_, i)));
                            }
                            continue;
                        }
                    }

                    ++i;

                    detail::hl::fold_n<Lexer::size()>(
                        string_view{}, [&](auto state, auto ci) {
                            if constexpr (!ci.value) {
                                return state;
                            }
                            if (parse_results.template get<ci.value>()) {
                                string_view const sv =
                                    parse_results.template get<ci.value>();
                                int const id = parent_->lexer_.ids()[ci.value];
                                constexpr detail::parse_spec parse_spec =
                                    parent_->lexer_.specs()[ci.value];
                                parent_->tokens_.push_back(
                                    detail::make_token<parse_spec>(
                                        id,
                                        sv,
                                        (ctre_first.current -
                                         ctre_first.orig_begin) -
                                            sv.size(),
                                        iterator(parent_, i)));
                                return sv;
                            } else {
                                return state;
                            }
                        });
                }

                parent_->latest_ = ctre_first.current;
            }

        public:
            iterator() = default;
            iterator(iterator const &) = default;
            iterator & operator=(iterator const &) = default;

            iterator(iterator<false> const & other)
                requires(Const)
                : parent_(other.parent_), token_offset_(other.token_offset_)
            {}

            iterator & operator=(iterator<false> const & other)
                requires(Const)
            {
                parent_ = other.parent_;
                token_offset_ = other.token_offset_;
            }

            // TODO: Document that lexeme/skip cause re-tokenization;
            // recommend using a token instead.

            iterator & operator++()
            {
                if (parent_->tokens_.size() <=
                    ++token_offset_ - parent_->base_token_offset_) {
                    fill_cache();
                }
                return *this;
            }

            token_type const & operator*() const
            {
                if ((BOOST_PARSER_TOKEN_POSITION_TYPE)parent_->tokens_.size() <=
                    token_offset_ - parent_->base_token_offset_) {
                    fill_cache();
                }
                BOOST_PARSER_DEBUG_ASSERT(
                    token_offset_ - parent_->base_token_offset_ <
                    (BOOST_PARSER_TOKEN_POSITION_TYPE)parent_->tokens_.size());
                return parent_
                    ->tokens_[token_offset_ - parent_->base_token_offset_];
            }

            bool operator==(iterator rhs) const
            {
                return parent_ == rhs.parent_ &&
                       token_offset_ == rhs.token_offset_;
            }

            bool operator==(iterator<!Const> rhs) const
            {
                return parent_ == rhs.parent_ &&
                       token_offset_ == rhs.token_offset_;
            }

            Parent & parent() const { return *parent_; }

            auto base() const
            {
                bool const at_end =
                    token_offset_ - parent_->base_token_offset_ ==
                    (BOOST_PARSER_TOKEN_POSITION_TYPE)parent_->tokens_.size();
                if (at_end) {
                    if constexpr (std::ranges::common_range<
                                      std::conditional_t<Const, V const, V>>) {
                        return range_end();
                    } else {
                        auto retval = range_begin();
                        if (!parent_->tokens_.empty()) {
                            retval +=
                                parent_->tokens_.back().underlying_position();
                        }
                        std::ranges::advance(retval, range_end());
                        return retval;
                    }
                } else {
                    return range_begin() + (**this).underlying_position();
                }
            }

            auto range_begin() const
            {
                return std::ranges::begin(parent_->base_);
            }
            auto range_end() const { return std::ranges::end(parent_->base_); }

            using base_type = detail::stl_interfaces::proxy_iterator_interface<
                iterator<Const>,
                std::forward_iterator_tag,
                token_type>;
            using base_type::operator++;
        };

        template<bool Const>
        class sentinel
        {
        private:
            using Parent = detail::maybe_const<Const, tokens_view>;
            using Base =
                std::conditional_t<Const, const_tokens_type, tokens_type>;

        public:
            sentinel() = default;
            constexpr sentinel(sentinel<!Const> i) {}

            template<bool OtherConst>
                requires std::sentinel_for<
                    std::ranges::sentinel_t<Base>,
                    std::ranges::iterator_t<std::conditional_t<
                        OtherConst,
                        const_tokens_type,
                        tokens_type>>>
            friend constexpr bool
            operator==(iterator<OtherConst> const & x, const sentinel & y)
            {
                return y.equal_to(x);
            }

        private:
            template<bool OtherConst>
            bool equal_to(iterator<OtherConst> const & x) const
            {
                if (x.token_offset_ - x.parent_->base_token_offset_ !=
                    x.parent_->tokens_.size()) {
                    return false;
                }
                auto r = std::ranges::subrange(
                    x.parent_->latest_, std::ranges::end(x.parent_->base_));
                auto ctre_range = Lexer::regex_range(r);
                return !ctre_range.begin().current_match;
            }
        };
#endif
    };

    template<typename R, typename Lexer>
    tokens_view(R &&, Lexer) -> tokens_view<std::views::all_t<R>, Lexer>;
    template<typename R, typename Lexer, typename TokenCache>
    tokens_view(R &&, Lexer, std::reference_wrapper<TokenCache>)
        -> tokens_view<std::views::all_t<R>, Lexer, TokenCache>;

    namespace detail {
        template<typename R, typename Lexer>
        concept can_tokens_view =
            requires { tokens_view(std::declval<R>(), Lexer()); };

        struct to_tokens_impl
        {
            template<parsable_range R, typename Lexer>
                requires std::ranges::viewable_range<R>
            [[nodiscard]] constexpr auto operator()(R && r, Lexer lexer) const
            {
                using T = detail::remove_cv_ref_t<R>;
                if constexpr (std::is_bounded_array_v<T>) {
                    constexpr auto n = std::extent_v<T>;
                    auto const offset = n && !r[n - 1] ? 1 : 0;
                    return tokens_view(
                        BOOST_PARSER_DETAIL_TEXT_SUBRANGE(
                            std::begin(r), std::end(r) - offset),
                        lexer);
                } else {
                    return tokens_view((R &&)r, lexer);
                }
            }

            template<parsable_range R, typename Lexer, typename TokenCache>
                requires std::ranges::viewable_range<R>
            [[nodiscard]] constexpr auto operator()(
                R && r,
                Lexer lexer,
                std::reference_wrapper<TokenCache> cache) const
            {
                using T = detail::remove_cv_ref_t<R>;
                if constexpr (std::is_bounded_array_v<T>) {
                    constexpr auto n = std::extent_v<T>;
                    auto const offset = n && !r[n - 1] ? 1 : 0;
                    return tokens_view(
                        BOOST_PARSER_DETAIL_TEXT_SUBRANGE(
                            std::begin(r), std::end(r) - offset),
                        lexer,
                        cache);
                } else {
                    return tokens_view((R &&)r, lexer, cache);
                }
            }
        };
    }

    /** A range adaptor that produces `boost::parser::token_view`s.  Takes a
        range (possibly using pipe syntax) as the first argument.  The second
        argument is the lexer to use.  The third argument is a
        `std::reference_wrapper<TokenCache>`, where `TokenCache` is a
        random-access container used to cache tokens during token parsing;
        this argument is optional. */
    inline constexpr detail::stl_interfaces::adaptor<detail::to_tokens_impl>
        to_tokens = detail::to_tokens_impl{};

}}

#endif
