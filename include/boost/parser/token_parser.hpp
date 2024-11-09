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
    }

#ifndef BOOST_PARSER_DOXYGEN

    // TODO: Needs a printer.
    template<typename AttributeType>
    struct token_parser
    {
        using attribute_type = std::conditional_t<
            std::is_same_v<AttributeType, void>,
            detail::nope,
            AttributeType>;

        constexpr token_parser() {}

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
            // TODO: Test for equality with some expectation, if any.
            auto opt_attr = detail::token_as<attribute_type>(x);
            if (!opt_attr) {
                success = false;
                return;
            }
            detail::assign(retval, *opt_attr);
            ++first;
        }
    };

#endif

    /** TODO */
    constexpr parser_interface<token_parser<detail::nope>> tok;

}}

#endif
#endif
