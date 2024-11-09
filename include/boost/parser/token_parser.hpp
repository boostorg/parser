// Copyright (C) 2024 T. Zachary Laine
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
#ifndef BOOST_PARSER_TOKEN_PARSER_HPP
#if defined(BOOST_PARSER_PARSER_HPP) && defined(BOOST_PARSER_LEXER_HPP)
#define BOOST_PARSER_TOKEN_PARSER_HPP

#include <boost/parser/parser_fwd.hpp>
#include <boost/parser/concepts.hpp>
#include <boost/parser/error_handling.hpp>


namespace boost { namespace parser {

    namespace detail {
        template<typename AttributeType, typename CharType>
        std::optional<AttributeType> token_as(token<CharType> tok)
        {
            if constexpr (std::same_as<AttributeType, nope>) {
                return nope{};
            } else if constexpr (std::same_as<
                                     AttributeType,
                                     std::basic_string_view<CharType>>) {
                if (tok.has_string_view())
                    return tok.get_string_view();
                return std::nullopt;
            } else if constexpr (std::is_floating_point_v<AttributeType>) {
                if (tok.has_long_double())
                    return tok.get_long_double();
                return std::nullopt;
            } else if constexpr (std::is_integral_v<AttributeType>) {
                if (tok.has_long_long())
                    return AttributeType(tok.get_long_long());
                return std::nullopt;
            } else {
                static_assert(
                    !std::same_as<CharType, CharType>,
                    "The only attribute types that can be pulled out of a "
                    "token are no-attribute, floating-point values, or "
                    "integral values (including charater types).");
            }
        }

        struct token_with_id
        {
            explicit token_with_id(int id) : id_(id) {}

            bool matches(int id) const { return id == id_; }

            template<typename T>
            bool matches_value(T) const
            {
                return true;
            }

            int id_;
        };

        template<typename T>
        struct token_with_id_and_value
        {
            explicit token_with_id_and_value(int id, T value) :
                id_(id), value_(value)
            {}

            bool matches(int id) const { return id == id_; }
            bool matches_value(T value) const { return value == value_; }

            int id_;
            T value_;
        };
    }

#ifndef BOOST_PARSER_DOXYGEN

    // TODO: Needs a printer.
    // TODO: Constrain the AttributeType to something that detail::token_as()
    // can handle.
    template<typename AttributeType, typename Expected>
    struct token_parser
    {
        using attribute_type = std::conditional_t<
            std::is_same_v<AttributeType, void>,
            detail::nope,
            AttributeType>;

        using expected_value_type = std::conditional_t<
            std::is_same_v<attribute_type, token_tag>,
            detail::nope,
            attribute_type>;

        constexpr token_parser() = default;
        constexpr token_parser(Expected expected) : expected_(expected) {}

        template<
            typename Iter,
            typename Sentinel,
            typename Context,
            typename SkipParser>
        auto call(
            Iter & first,
            Sentinel last,
            Context const & context,
            SkipParser const & skip,
            detail::flags flags,
            bool & success) const -> attribute_type
        {
            attribute_type retval;
            call(first, last, context, skip, flags, success, retval);
            return retval;
        }

        template<
            typename Iter,
            typename Sentinel,
            typename Context,
            typename SkipParser,
            typename Attribute>
        void call(
            Iter & first,
            Sentinel last,
            Context const & context,
            SkipParser const & skip,
            detail::flags flags,
            bool & success,
            Attribute & retval) const
        {
            using value_type = std::remove_cvref_t<decltype(*first)>;
            static_assert(
                is_token_v<value_type>,
                "token_parser can only be used when parsing sequences of "
                "tokens.");

            [[maybe_unused]] auto _ = detail::scoped_trace(
                *this, first, last, context, flags, retval);

            if (first == last) {
                success = false;
                return;
            }

            value_type const x = *first;
            if (!expected_.matches_id(x.id())) {
                success = false;
                return;
            }

            if constexpr (std::same_as<AttributeType, token_tag>) {
                detail::assign(retval, x);
            } else {
                auto opt_attr = detail::token_as<attribute_type>(x);
                if (!opt_attr || !expected_.matches_value(*opt_attr)) {
                    success = false;
                    return;
                }
                detail::assign(retval, *opt_attr);
            }

            ++first;
        }

        // TODO: token_spec_t needs these same operator() overloads, each of
        // which will return a token_parser.

        // TODO: Constrain both ID params below only to accept type
        // convertible to int.

        /** TODO */
        template<typename ID>
        constexpr auto operator()(ID id) const noexcept
        {
            BOOST_PARSER_ASSERT(
                (detail::is_nope_v<Expected> &&
                 "If you're seeing this, you tried to chain calls on tok or "
                 "tok_t, like 'tok(id1)(id2)'.  Quit it!'"));
            return parser_interface(detail::token_with_id((int)id));
        }

        /** TODO */
        template<typename ID>
        constexpr auto
        operator()(ID id, expected_value_type value) const noexcept
        {
            BOOST_PARSER_ASSERT(
                (detail::is_nope_v<Expected> &&
                 "If you're seeing this, you tried to chain calls on tok or "
                 "tok_t, like 'tok(id1)(id2)'.  Quit it!'"));
            return parser_interface(
                detail::token_with_id_and_value((int)id, value));
        }

        Expected expected_;
    };

#endif

    /** TODO */
    constexpr parser_interface<token_parser<>> tok;

    /** TODO */
    template<typename AttributeType>
    constexpr parser_interface<token_parser<AttributeType>> tok_t;

}}

#endif
#endif
