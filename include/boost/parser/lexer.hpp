// Copyright (C) 2024 T. Zachary Laine
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
#ifndef BOOST_PARSER_LEXER_HPP
#define BOOST_PARSER_LEXER_HPP

#include <boost/parser/config.hpp>
#include <boost/parser/detail/debug_assert.hpp>
#include <boost/parser/detail/hl.hpp>
#include <boost/parser/detail/numeric.hpp>
#include <boost/parser/detail/text/transcode_view.hpp>

#if !BOOST_PARSER_USE_CONCEPTS || !__has_include(<ctre-unicode.hpp>)
#error                                                                         \
    "In order to work, the Boost.Parser lexer requires C++20 and CTRE's ctre-unicode.hpp single-header file in the #include path.  CTRE can be found at https://github.com/hanickadot/compile-time-regular-expressions .  The required header is at https://raw.githubusercontent.com/hanickadot/compile-time-regular-expressions/refs/heads/main/single-header/ctre-unicode.hpp ."
#endif

#include <ctre-unicode.hpp>

#if defined(BOOST_PARSER_TESTING)
#include <iostream>
#endif
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

    struct none;

#if 1 // TODO
    struct none
    {};
#endif

    namespace detail {
        enum class token_kind { no_value, string_view, long_long, double_ };

        enum class token_parsed_type {
            ws,

            character,

            string_view,

            bool_,

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

    inline constexpr int ws_id = -1000000;
    inline constexpr int character_id = -2000000;

    /** TODO */
    template<typename CharType>
    struct token
    {
        using char_type = CharType;
        using string_view = std::basic_string_view<CharType>;

        constexpr token() :
            value_(0ll), id_(), kind_(detail::token_kind::string_view)
        {}
        constexpr token(int id, string_view value) :
            value_(0ll), id_(id), kind_(detail::token_kind::string_view)
        {
            value_.sv_ = value;
        }
        constexpr token(int id, long long value) :
            value_(0ll), id_(id), kind_(detail::token_kind::long_long)
        {
            value_.ll_ = value;
        }
        constexpr token(int id, double value) :
            value_(0ll), id_(id), kind_(detail::token_kind::double_)
        {
            value_.d_ = value;
        }

        constexpr int id() const { return id_; }

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

        constexpr bool has_double() const
        {
            return kind_ == detail::token_kind::double_;
        }
        constexpr double get_double() const
        {
            BOOST_PARSER_DEBUG_ASSERT(kind_ == detail::token_kind::double_);
            return value_.d_;
        }

        constexpr bool operator==(token const & rhs) const
        {
            if (id_ != rhs.id_ || (kind_ != rhs.kind_ &&
                                   rhs.kind_ != detail::token_kind::no_value)) {
                return false;
            }
            switch (rhs.kind_) {
            case detail::token_kind::no_value: return true;
            case detail::token_kind::string_view:
                return get_string_view() == rhs.get_string_view();
            case detail::token_kind::long_long:
                return get_long_long() == rhs.get_long_long();
            case detail::token_kind::double_:
                return get_double() == rhs.get_double();
            default:
                BOOST_PARSER_DEBUG_ASSERT(!"Error: invalid token kind.");
#if defined(__cpp_lib_unreachable)
                std::unreachable();
#endif
                return false;
            }
        }

        constexpr bool operator==(char32_t rhs) const
        {
            return id() == character_id && has_long_long() &&
                   get_long_long() == rhs;
        }

    private:
        union value
        {
            long long ll_;
            double d_;
            string_view sv_;
        } value_;
        int id_;
        detail::token_kind kind_;
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
        } else if (token.has_double()) {
            os << token.get_double();
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

    // TODO: Actually, this should go in parse.hpp.
    /** TODO */
    template<typename CharType, typename Attr = none>
    struct token_parser
    {
        // TODO
    };

    template<typename T>
    concept character_type = std::same_as<T, char> || std::same_as<T, char8_t>;

    namespace detail {
        template<typename TokenSpec>
        constexpr parse_spec parse_spec_for()
        {
            using value_t = typename TokenSpec::value_type;
            constexpr auto base = TokenSpec::base;
            if constexpr (TokenSpec::is_character_token) {
                return parse_spec{token_parsed_type::character, base};
            } else if constexpr (std::is_same_v<value_t, none>) {
                return parse_spec{token_parsed_type::string_view, base};
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
                    !std::is_same_v<value_t, value_t>,
                    "The only valid types for the 'Value' template parameter "
                    "to 'lexer_token_spec' are 'none', integral types, and "
                    "floating-point types.");
            }
        }

        template<typename T>
        token_kind token_kind_for()
        {
            if constexpr (std::is_same_v<T, none>) {
                return token_kind::string_view;
            } else if constexpr (std::is_same_v<T, long long>) {
                return token_kind::long_long;
            } else if constexpr (std::is_same_v<T, double>) {
                return token_kind::double_;
            } else {
                static_assert(
                    !std::is_same_v<T, T>,
                    "The only valid types for the 'Value' template parameter "
                    "to 'lexer_token_spec' are 'none', 'long long', and "
                    "'double'.");
            }
        }

        template<auto Ch, auto... Chs>
        struct token_chars_spec
        {
            static_assert(
                (std::same_as<decltype(Ch), decltype(Chs)> && ... && true),
                "All non-type template parameters given to token_chars_spec "
                "must be chars.");

            static_assert(
                (unsigned char)Ch < 128 &&
                    ((unsigned char)(Chs < 128) && ... && true),
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

    /** TODO */
    template<
        ctll::fixed_string Regex,
        auto ID,
        typename ValueType = none,
        int Base = 10>
    struct token_spec_t
    {
        using id_type = decltype(ID);
        using value_type = ValueType;

        static_assert(
            0 <= (int)ID, "Token IDs must be integral values or enums >=0.");

        // TODO: Check for subgroups within Regex.  That is, make sure that
        // every '(' seen is followed immediately by a '?'.

        static constexpr ctll::fixed_string regex = Regex;
        static constexpr id_type id = ID;
        static constexpr int base = Base < 0 ? 10 : Base;
        static constexpr bool is_character_token = Base < 0;
    };

    /** TODO */
    template<
        ctll::fixed_string Regex,
        auto ID,
        typename ValueType = none,
        int Base = 10>
    constexpr auto token_spec = token_spec_t<Regex, ID, ValueType, Base>{};

    /** TODO */
    template<char Ch, auto... Chs>
    constexpr auto token_chars = detail::token_chars_spec<Ch, Chs...>{};

    // TODO: Document that the ID type given to the inital lexer<>() is the one
    // that must be used for all non-character token specs.
    /** TODO */
    // TODO: Indicate in the docs that using the lexer implies using these
    // allocating variables.
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

        template<
            ctll::fixed_string RegexStr2,
            auto ID2,
            typename ValueType,
            int Base>
        auto operator|(token_spec_t<RegexStr2, ID2, ValueType, Base> rhs) const
        {
            static_assert(
                std::same_as<ID, decltype(ID2)>,
                "All id_types must be the same for all token_specs.");
            constexpr auto new_regex =
                detail::wrap_escape_concat<regex_str, RegexStr2>();
            constexpr auto new_ids = IDs.template append<(int)ID2>();
            constexpr auto new_specs =
                Specs
                    .template append<detail::parse_spec_for<decltype(rhs)>()>();
            return lexer_t<
                CharType,
                ID,
                WsStr,
                new_regex,
                new_ids,
                new_specs>{};
        }

        template<CharType Ch, auto... Chs>
        auto operator|(detail::token_chars_spec<Ch, Chs...> rhs) const
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

        template<typename V>
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

    /** TODO */
    template<
        typename CharType,
        typename ID,
        ctll::fixed_string WsStr = "\\s+",
        ctll::fixed_string RegexStr = "",
        detail::nttp_array IDs = detail::nttp_array<-1>{},
        detail::nttp_array Specs = detail::nttp_array<detail::parse_spec{}>{}>
    constexpr auto lexer = lexer_t<CharType, ID, WsStr, RegexStr, IDs, Specs>{};

    // TODO: Document that every spec's chars need to be in the same UTF (or
    // none).

    namespace detail {
        template<typename T>
        struct wrapper
        {
            using type = T;
        };

        template<parse_spec Spec, typename CharType>
        token<CharType>
        make_token(int id, std::basic_string_view<CharType> ctre_token)
        {
            auto f = ctre_token.data();
            auto const l = f + ctre_token.size();

            // radix==0 indicates a real number was parsed.
            auto report_error = [](auto type, int radix, bool success) {
                if (!success)
                    ; // TODO: report error.
            };

            switch (Spec.type) {
            case token_parsed_type::character:
                return {character_id, (long long)ctre_token[0]};

            case token_parsed_type::string_view: return {id, ctre_token};

            case token_parsed_type::bool_:
                if (ctre_token == "true") {
                    return {id, 1ll};
                } else if (ctre_token == "false") {
                    return {id, 0ll};
                } else {
                    // TODO: report error.
                }

            case token_parsed_type::signed_char: {
                signed char value;
                report_error(
                    wrapper<decltype(value)>{},
                    Spec.radix,
                    numeric::parse_int<true, Spec.radix, 1, -1>(f, l, value));
                return {id, (long long)value};
            }
            case token_parsed_type::unsigned_char: {
                unsigned char value;
                report_error(
                    wrapper<decltype(value)>{},
                    Spec.radix,
                    numeric::parse_int<false, Spec.radix, 1, -1>(f, l, value));
                return {id, (long long)value};
            }
            case token_parsed_type::short_: {
                short value;
                report_error(
                    wrapper<decltype(value)>{},
                    Spec.radix,
                    numeric::parse_int<true, Spec.radix, 1, -1>(f, l, value));
                return {id, (long long)value};
            }
            case token_parsed_type::unsigned_short: {
                unsigned short value;
                report_error(
                    wrapper<decltype(value)>{},
                    Spec.radix,
                    numeric::parse_int<false, Spec.radix, 1, -1>(f, l, value));
                return {id, (long long)value};
            }
            case token_parsed_type::int_: {
                int value;
                report_error(
                    wrapper<decltype(value)>{},
                    Spec.radix,
                    numeric::parse_int<true, Spec.radix, 1, -1>(f, l, value));
                return {id, (long long)value};
            }
            case token_parsed_type::unsigned_int: {
                unsigned int value;
                report_error(
                    wrapper<decltype(value)>{},
                    Spec.radix,
                    numeric::parse_int<false, Spec.radix, 1, -1>(f, l, value));
                return {id, (long long)value};
            }
            case token_parsed_type::long_: {
                long value;
                report_error(
                    wrapper<decltype(value)>{},
                    Spec.radix,
                    numeric::parse_int<true, Spec.radix, 1, -1>(f, l, value));
                return {id, (long long)value};
            }
            case token_parsed_type::unsigned_long: {
                unsigned long value;
                report_error(
                    wrapper<decltype(value)>{},
                    Spec.radix,
                    numeric::parse_int<false, Spec.radix, 1, -1>(f, l, value));
                return {id, (long long)value};
            }
            case token_parsed_type::long_long: {
                long long value;
                report_error(
                    wrapper<decltype(value)>{},
                    Spec.radix,
                    numeric::parse_int<true, Spec.radix, 1, -1>(f, l, value));
                return {id, (long long)value};
            }
            case token_parsed_type::unsigned_long_long: {
                unsigned long long value;
                report_error(
                    wrapper<decltype(value)>{},
                    Spec.radix,
                    numeric::parse_int<false, Spec.radix, 1, -1>(f, l, value));
                return {id, (long long)value};
            }
            case token_parsed_type::wchar_t_: {
                unsigned int value;
                report_error(
                    wrapper<wchar_t>{},
                    Spec.radix,
                    numeric::parse_int<false, Spec.radix, 1, -1>(f, l, value));
                return {id, (long long)value};
            }
            case token_parsed_type::char8_t_: {
                unsigned int value;
                report_error(
                    wrapper<char8_t>{},
                    Spec.radix,
                    numeric::parse_int<false, Spec.radix, 1, -1>(f, l, value));
                return {id, (long long)value};
            }
            case token_parsed_type::char16_t_: {
                unsigned int value;
                report_error(
                    wrapper<char16_t>{},
                    Spec.radix,
                    numeric::parse_int<false, Spec.radix, 1, -1>(f, l, value));
                return {id, (long long)value};
            }
            case token_parsed_type::char32_t_: {
                unsigned int value;
                report_error(
                    wrapper<char32_t>{},
                    Spec.radix,
                    numeric::parse_int<false, Spec.radix, 1, -1>(f, l, value));
                return {id, (long long)value};
            }

            case token_parsed_type::float_: {
                float value;
                report_error(
                    wrapper<decltype(value)>{},
                    0,
                    numeric::parse_real(f, l, value));
                return {id, (double)value};
            }
            case token_parsed_type::double_: {
                double value;
                report_error(
                    wrapper<decltype(value)>{},
                    0,
                    numeric::parse_real(f, l, value));
                return {id, (double)value};
            }
            case token_parsed_type::long_double: {
                long double value;
                report_error(
                    wrapper<decltype(value)>{},
                    0,
                    numeric::parse_real(f, l, value));
                return {id, (double)value};
            }
            case token_parsed_type::ws:
            default:
#if defined(__cpp_lib_unreachable)
                std::unreachable();
#endif
                return {id, 0ll};
            }
        }
    }

    namespace detail {
        template<bool Const, typename T>
        using maybe_const = std::conditional_t<Const, T const, T>;
    }

    template<std::ranges::forward_range V, typename Lexer>
        requires std::ranges::view<V>
    struct token_view : public std::ranges::view_interface<token_view<V, Lexer>>
    {
    private:
        template<bool>
        struct iterator;

        template<bool>
        struct sentinel;

        template<bool>
        friend struct iterator;

        using tokens_type = decltype(Lexer::regex_range(std::declval<V &>()));

    public:
        using token_type = typename Lexer::token_type;

        token_view()
            requires std::default_initializable<V>
            : base_(), lexer_(), tokens_(Lexer::regex_range(base_))
        {}
        constexpr explicit token_view(V base, Lexer lexer) :
            base_(std::move(base)),
            lexer_(std::move(lexer)),
            tokens_(Lexer::regex_range(base_))
        {}

        constexpr V base() const &
            requires std::copy_constructible<V>
        {
            return base_;
        }
        constexpr V base() && { return std::move(base_); }

        constexpr Lexer lexer() { return lexer_; }

        constexpr iterator<false> begin()
        {
            return iterator<false>(*this, tokens_.begin());
        }
        constexpr iterator<true> begin() const
            requires std::ranges::range<const tokens_type>
        {
            return iterator<true>(*this, tokens_.begin());
        }

        constexpr sentinel<false> end()
        {
            return sentinel<false>(tokens_.end());
        }
        constexpr sentinel<true> end() const
            requires std::ranges::range<const tokens_type>
        {
            return sentinel<true>(tokens_.end());
        }

    private:
        V base_ = V();
        Lexer lexer_;
        tokens_type tokens_;

        template<bool Const>
        struct iterator
            : public detail::stl_interfaces::proxy_iterator_interface<
                  iterator<Const>,
                  detail::text::detail::iterator_to_tag_t<
                      std::ranges::iterator_t<
                          detail::maybe_const<Const, tokens_type>>>,
                  token_type>
        {
        private:
            using base_iterator_type = std::ranges::iterator_t<
                detail::maybe_const<Const, tokens_type>>;

            friend detail::stl_interfaces::access;
            base_iterator_type & base_reference() noexcept { return current_; }
            base_iterator_type base_reference() const { return current_; }

            token_view * parent_;
            base_iterator_type current_ = base_iterator_type();

            friend token_view::sentinel<Const>;

        public:
            constexpr iterator() = default;
            constexpr iterator(token_view & parent, base_iterator_type it) :
                parent_(&parent), current_(std::move(it))
            {}

            // TODO: Needs caching of tokens.  Don't forget to allow the token
            // cache to be user-supplied.

            constexpr token_type operator*() const
            {
                using string_view = typename Lexer::string_view;

                token_type retval;

                auto const parse_results = *current_;

#if 1
                if constexpr (Lexer::has_ws) {
                    if (auto sv =
                            parse_results.template get<Lexer::size()>()) {
                        retval = token_type(ws_id, sv);
                        return retval; // TODO: Skip this ws token instead.
                    }
                }
#endif

                detail::hl::fold_n<Lexer::size()>(
                    string_view{}, [&](auto state, auto i) {
                        if constexpr (!i.value) {
                            return state;
                        }
                        if (parse_results.template get<i.value>()) {
                            string_view const sv =
                                parse_results.template get<i.value>();
                            if constexpr (i.value == Lexer::size()) {
                                retval = token_type(ws_id, sv);
                            } else {
                                int const id = parent_->lexer_.ids()[i.value];
                                constexpr detail::parse_spec parse_spec =
                                    parent_->lexer_.specs()[i.value];
                                retval = detail::make_token<parse_spec>(id, sv);
                            }
                            return sv;
                        } else {
                            return state;
                        }
                    });
                return retval;
            }
        };

        template<bool Const>
        class sentinel
        {
        private:
            using Parent = detail::maybe_const<Const, token_view>;
            using Base = detail::maybe_const<Const, tokens_type>;

        public:
            sentinel() = default;
            constexpr explicit sentinel(std::ranges::sentinel_t<Base> end) :
                end_(end)
            {}
            constexpr sentinel(sentinel<!Const> i)
                requires Const && std::convertible_to<
                                      std::ranges::sentinel_t<tokens_type>,
                                      std::ranges::sentinel_t<Base>>
                : end_(std::move(end))
            {}

            constexpr std::ranges::sentinel_t<Base> base() const
            {
                return end_;
            }

            template<bool OtherConst>
                requires std::sentinel_for<
                    std::ranges::sentinel_t<Base>,
                    std::ranges::iterator_t<
                        detail::maybe_const<OtherConst, tokens_type>>>
            friend constexpr bool
            operator==(const iterator<OtherConst> & x, const sentinel & y)
            {
                return x.current_ == y.end_;
            }

            template<bool OtherConst>
                requires std::sized_sentinel_for<
                    std::ranges::sentinel_t<Base>,
                    std::ranges::iterator_t<
                        detail::maybe_const<OtherConst, tokens_type>>>
            friend constexpr std::ranges::range_difference_t<
                detail::maybe_const<OtherConst, tokens_type>>
            operator-(const iterator<OtherConst> & x, const sentinel & y)
            {
                return x.current_ - y.end_;
            }

            template<bool OtherConst>
                requires std::sized_sentinel_for<
                    std::ranges::sentinel_t<Base>,
                    std::ranges::iterator_t<
                        detail::maybe_const<OtherConst, tokens_type>>>
            friend constexpr std::ranges::range_difference_t<
                detail::maybe_const<OtherConst, tokens_type>>
            operator-(const sentinel & y, const iterator<OtherConst> & x)
            {
                return y.end_ - x.current_;
            }

        private:
            std::ranges::sentinel_t<Base> end_ =
                std::ranges::sentinel_t<Base>();
        };
    };

    template<typename R, typename Lexer>
    token_view(R &&, Lexer) -> token_view<std::views::all_t<R>, Lexer>;

    // TODO: Needs Range adaptor.

}}

#endif
