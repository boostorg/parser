// Copyright (C) 2024 T. Zachary Laine
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
#ifndef BOOST_PARSER_LEXER_HPP
#define BOOST_PARSER_LEXER_HPP

#include <boost/parser/ctre-unicode.hpp>
#include <boost/parser/detail/debug_assert.hpp>

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
    }

    /** TODO */
    template<typename CharType>
    struct token
    {
        using char_type = CharType;
        using string_view = std::basic_string_view<CharType>;

        constexpr token() : id_(0), kind_(detail::token_kind::string_view) {}
        constexpr token(int id, string_view value) :
            id_(id), kind_(detail::token_kind::string_view)
        {
            value_.sv_ = value;
        }
        constexpr token(int id, long long value) :
            id_(id), kind_(detail::token_kind::long_long)
        {
            value_.ll_ = value;
        }
        constexpr token(int id, double value) :
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
        int id_;
        detail::token_kind kind_;
    };

    namespace detail {
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
    }

    /** TODO */
    template<auto & Regex, typename Value = none>
    struct lexer_token_spec
    {
        static_assert(
            std::is_same_v<Value, none> || std::is_same_v<Value, long long> ||
            std::is_same_v<Value, double>);

        using value_type = Value;

        explicit lexer_token_spec(int id) : id(id) {}

        static constexpr auto & regex = Regex;

        int id;
    };

#ifdef BOOST_PARSER_DOXYGEN

    /** TODO */
    template<typename Regex>
    struct lexer;

#else

    // TODO: Indicate in the docs that using the lexer implies using these
    // allocating variables.
    template<typename Regex, int Count>
    struct lexer
    {
        Regex regex;
        std::vector<int> ids; // TODO: reintroduce IDType.
        std::vector<detail::token_kind> value_types;
    };

#endif

    /** TODO */
    template<typename T, typename... Ts>
    auto make_lexer(T const & x, Ts const &... xs)
    {
        auto regex = (ctre::re<T::regex>() | ... | ctre::re<Ts::regex>());
        return lexer<decltype(regex), sizeof...(xs) + 1>{
            regex,
            {detail::token_kind_for<typename T::value_type>(),
             detail::token_kind_for<typename Ts::value_type>()...},
            {T::id, Ts::id...}};
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
            hl::fold_n<Count>(
                std::string_view{},
                [&](auto state, auto i) -> std::string_view {
                    if (results.get<i.value>())
                        return results.get<i.value>();
                    else
                        return state;
                });
        }
    }
#endif

}}

#endif
