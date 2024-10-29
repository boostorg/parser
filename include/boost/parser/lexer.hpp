// Copyright (C) 2024 T. Zachary Laine
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
#ifndef BOOST_PARSER_LEXER_HPP
#define BOOST_PARSER_LEXER_HPP

#include <boost/parser/config.hpp>
#include <boost/parser/detail/debug_assert.hpp>
#include <boost/parser/detail/numeric.hpp>

#if !BOOST_PARSER_USE_CONCEPTS || !__has_include(<ctre-unicode.hpp>)
#error                                                                         \
    "In order to work, the Boost.Parser lexer requires C++20 and CTRE's ctre-unicode.hpp single-header file in the #include path.  CTRE can be found at https://github.com/hanickadot/compile-time-regular-expressions .  The required header is at https://raw.githubusercontent.com/hanickadot/compile-time-regular-expressions/refs/heads/main/single-header/ctre-unicode.hpp ."
#endif

#include <ctre-unicode.hpp>

#include <string_view>
#include <type_traits>
#include <vector>


namespace boost { namespace parser {

    struct none;

#if 1 // TODO
    struct none
    {};
#endif

    namespace detail {
        enum class token_kind { no_value, string_view, long_long, double_ };

        enum class token_parsed_type {
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

        struct parse_spec
        {
            token_parsed_type type;
            int base;
        };
    }

    /** TODO */
    template<typename CharType, typename ID>
    struct token
    {
        using char_type = CharType;
        using id_type = ID;
        using string_view = std::basic_string_view<CharType>;

        constexpr token() : id_(0), kind_(detail::token_kind::string_view) {}
        constexpr token(id_type id, string_view value) :
            id_(id), kind_(detail::token_kind::string_view)
        {
            value_.sv_ = value;
        }
        constexpr token(id_type id, long long value) :
            id_(id), kind_(detail::token_kind::long_long)
        {
            value_.ll_ = value;
        }
        constexpr token(id_type id, double value) :
            id_(id), kind_(detail::token_kind::double_)
        {
            value_.d_ = value;
        }

        constexpr bool id() const { return id_; }

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

    private:
        union value
        {
            string_view sv_;
            long long ll_;
            double d_;
        } value_;
        id_type id_;
        detail::token_kind kind_;
    };

    /** TODO */
    template<typename CharType, typename Attr = none>
    struct token_parser
    {
        // TODO
    };

    namespace detail {
        template<typename TokenSpec>
        parse_spec parse_spec_for()
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

        template<
            ctll::fixed_string Regex,
            typename ID,
            typename Value,
            int Base>
        struct token_spec
        {
            using id_type = ID;
            using value_type = Value;

            static constexpr ctll::fixed_string regex = Regex;
            static constexpr int base = Base < 0 ? 10 : Base;
            static constexpr bool is_character_token = Base < 0;

            explicit token_spec(id_type id) : id(id) {}
            explicit token_spec(id_type id, value_type value) :
                id(id), value(value)
            {}

            id_type id;
            value_type value;
        };
    }

    template<typename T>
    concept character_type =
        std::same_as<T, char> || std::same_as<T, wchar_t> ||
        std::same_as<T, char8_t> || std::same_as<T, char16_t> ||
        std::same_as<T, char32_t>;

    namespace detail {
        inline constexpr int char_id_offset = (1 << 30);

        template<auto Regex, typename Value, int Base>
        struct token_spec_facade
        {
            template<typename ID>
            constexpr auto operator()(ID id)
            {
                using regex_t = decltype(Regex);
                if constexpr (character_type<regex_t>) {
                    // TODO: Document this, and note that surrogate pairs and
                    // multicharacter UTF encodings of code points must use
                    // the non-char token_spec form.
                    if constexpr (Regex <= 0x10ffff) {
                        constexpr regex_t regex_str[2] = {Regex, 0};
                        BOOST_PARSER_DIAGNOSTIC_PUSH
#if defined(_MSC_VER)
// TODO #pragma warning(disable: 26451)
#elif defined(__clang_major__)
#pragma clang diagnostic ignored "-Woverflow"
#elif defined(__GNUC__)
#pragma GCC diagnostic ignored "-Woverflow"
#endif
                        return token_spec<regex_str, ID, int, Base>(
                            char_id_offset + Regex);
                        BOOST_PARSER_DIAGNOSTIC_POP
                    } else {
                        throw std::runtime_error(
                            "Character token values must not exceed the "
                            "maximum Unicode code point value.");
                    }
                } else {
                    return token_spec<Regex, ID, Value, Base>(id);
                }
            }
        };
    }

    /** TODO */
    template<ctll::fixed_string Regex, typename Value = none, int Base = 10>
    auto token_spec = detail::token_spec_facade<Regex, Value, Base>{};

    /** TODO */
    template<auto Ch>
        requires character_type<decltype(Ch)>
    auto char_token =
        detail::token_spec_facade<Ch, decltype(Ch), 10>{}((decltype(Ch))0);

#if defined(BOOST_PARSER_DOXYGEN)

    /** TODO */
    template<typename Regex>
    struct lexer;

#else

    // TODO: Indicate in the docs that using the lexer implies using these
    // allocating variables.
    template<typename Regex, typename ID, int Count>
    struct lexer
    {
        Regex regex;
        std::vector<int> ids; // TODO: non-character tokens' IDs must be cast to
                              // ID before calling make_token().
        std::vector<detail::parse_spec> value_types;
    };

#endif

    // TODO: Document that every T before needs to be in the same UTF (or
    // none).

    /** TODO */
    template<typename T, typename... Ts>
    auto make_lexer(T const & x, Ts const &... xs)
    {
        static_assert(
            ((character_type<typename Ts::id_type> ||
              std::same_as<typename T::id_type, typename Ts::id_type>) &&
             ... && true),
            "All id_types must be the same for all token_specs.");

        // Check that id_type is an enum, or an integral type.
        // Check that the IDs are never repeated.

        auto regex = (ctre::re<T::regex>() | ... | ctre::re<Ts::regex>());
        return lexer<decltype(regex), typename T::id_type, sizeof...(Ts) + 1>{
            regex,
            {(int)x.id, (int)xs.id...},
            {detail::parse_spec_for<T>(), detail::parse_spec_for<Ts>()...}};
    }

    namespace detail {
        template<typename CharType, typename ID>
        token<CharType, ID> make_token(
            ID id, std::basic_string_view<CharType> ctre_token, parse_spec spec)
        {
            auto f = ctre_token.data();
            auto const l = f + ctre_token.size();

            auto parse_int = [](auto f, auto l, auto & value) {
                bool const success = numeric::parse_int(f, l, value);
                if (!success)
                    ; // TODO: report error.
            };
            auto parse_real = [](auto f, auto l, auto & value) {
                bool const success = numeric::parse_int(f, l, value);
                if (!success)
                    ; // TODO: report error.
            };

            switch (spec.type) {
            case token_parsed_type::character: return {id, ctre_token[0]};

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
                parse_int(f, l, value);
                return {id, ctre_token};
            }
            case token_parsed_type::unsigned_char: {
                unsigned char value;
                parse_int(f, l, value);
                return {id, ctre_token};
            }
            case token_parsed_type::short_: {
                short value;
                parse_int(f, l, value);
                return {id, ctre_token};
            }
            case token_parsed_type::unsigned_short: {
                unsigned short value;
                parse_int(f, l, value);
                return {id, ctre_token};
            }
            case token_parsed_type::int_: {
                int value;
                parse_int(f, l, value);
                return {id, ctre_token};
            }
            case token_parsed_type::unsigned_int: {
                unsigned int value;
                parse_int(f, l, value);
                return {id, ctre_token};
            }
            case token_parsed_type::long_: {
                long value;
                parse_int(f, l, value);
                return {id, ctre_token};
            }
            case token_parsed_type::unsigned_long: {
                unsigned long value;
                parse_int(f, l, value);
                return {id, ctre_token};
            }
            case token_parsed_type::long_long: {
                long long value;
                parse_int(f, l, value);
                return {id, ctre_token};
            }
            case token_parsed_type::unsigned_long_long: {
                unsigned long long value;
                parse_int(f, l, value);
                return {id, ctre_token};
            }
            case token_parsed_type::wchar_t_: {
                wchar_t value;
                parse_int(f, l, value);
                return {id, ctre_token};
            }
            case token_parsed_type::char8_t_: {
                char8_t value;
                parse_int(f, l, value);
                return {id, ctre_token};
            }
            case token_parsed_type::char16_t_: {
                char16_t value;
                parse_int(f, l, value);
                return {id, ctre_token};
            }
            case token_parsed_type::char32_t_: {
                char32_t value;
                parse_int(f, l, value);
                return {id, ctre_token};
            }

            case token_parsed_type::float_: {
                float value;
                parse_real(f, l, value);
                return {id, ctre_token};
            }
            case token_parsed_type::double_: {
                double value;
                parse_real(f, l, value);
                return {id, ctre_token};
            }
            case token_parsed_type::long_double: {
                long double value;
                parse_real(f, l, value);
                return {id, ctre_token};
            }
            }
        }
    }

#if 0
    // TODO: Jus' a sketch.
    void process_tokens(lexer<Regex, Count> lex, std::string_view input)
    {
        for (/*TODO while?*/) {
            auto results = ctre::match<"^([a-z]++)|([0-9]++)$">(input);
            if (!results) {
                // TODO: Report error.
            }
            auto [ctre_token, i] = hl::fold_n<Count>(
                std::pair(std::string_view{}, 0),
                [&](auto state, auto i) -> std::string_view {
                    if (results.get<i.value>())
                        return std::pair(results.get<i.value>(), (int)i);
                    else
                        return state;
                });
        }
    }
#endif

}}

#endif
