// Copyright (C) 2024 T. Zachary Laine
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
#ifndef BOOST_PARSER_TOKEN_PARSER_HPP
#define BOOST_PARSER_TOKEN_PARSER_HPP

#if !defined(BOOST_PARSER_PARSER_HPP) || !defined(BOOST_PARSER_LEXER_HPP)
#error "token_parser.hpp must be included after lexer.hpp and parser.hpp."
#endif

#include <boost/parser/parser_fwd.hpp>
#include <boost/parser/concepts.hpp>
#include <boost/parser/error_handling.hpp>

#include <algorithm>


namespace boost { namespace parser {

    namespace detail {
        template<typename AttributeType, typename CharType>
        std::optional<AttributeType> token_as(token<CharType> tok)
        {
            if constexpr (std::same_as<AttributeType, string_view_tag>) {
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

        template<typename T>
        struct token_with_value
        {
            explicit token_with_value(T value) : value_(value) {}
            bool matches(T value) const { return value == value_; }
            T value_;
        };

        template<typename Subrange>
        struct token_with_string_view
        {
            explicit token_with_string_view(Subrange value) : value_(value) {}

            template<typename CharType>
            bool matches(std::basic_string_view<CharType> value) const
            {
                return std::ranges::equal(value, value_);
            }

            Subrange value_;
        };
    }

#ifndef BOOST_PARSER_DOXYGEN

    // TODO: Constrain the AttributeType to something that detail::token_as()
    // can handle.
    template<typename TokenSpec, typename Expected>
    struct token_parser
    {
        using token_spec = TokenSpec;

        using attribute_type = std::conditional_t<
            std::is_same_v<typename token_spec::value_type, none>,
            string_view_tag,
            typename token_spec::value_type>;

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
            if (x.id() != token_spec::id) {
                success = false;
                return;
            }

            constexpr bool use_expected = !std::same_as<Expected, detail::nope>;
            if (use_expected || detail::gen_attrs(flags)) {
                auto opt_attr = detail::token_as<attribute_type>(x);
                if constexpr (use_expected) {
                    if (!opt_attr || !expected_.matches_value(*opt_attr)) {
                        success = false;
                        return;
                    }
                }
                if (detail::gen_attrs(flags))
                    detail::assign(retval, *opt_attr);
            }

            ++first;
        }

        /** TODO */
        constexpr auto operator()(attribute_type value) const noexcept
        {
            BOOST_PARSER_ASSERT(
                (detail::is_nope_v<Expected> &&
                 "If you're seeing this, you tried to chain calls on one of "
                 "your token_spec's, like 'my_token_spec(id1)(id2)'.  Quit "
                 "it!'"));
            return parser_interface(token_parser<
                                    TokenSpec,
                                    detail::token_with_value<attribute_type>>(
                detail::token_with_value(std::move(value))));
        }

        template<parsable_range_like R>
        constexpr auto operator()(R && r) const noexcept
        {
            BOOST_PARSER_ASSERT(
                ((!std::is_rvalue_reference_v<R &&> ||
                  !detail::is_range<detail::remove_cv_ref_t<R>>) &&
                 "It looks like you tried to pass an rvalue range to "
                 "token_spec().  Don't do that, or you'll end up with dangling "
                 "references."));
            BOOST_PARSER_ASSERT(
                (detail::is_nope_v<Expected> &&
                 "If you're seeing this, you tried to chain calls on "
                 "token_spec, like 'token_spec(char-set)(char-set)'.  Quit "
                 "it!'"));
            auto expected =
                detail::token_with_string_view{BOOST_PARSER_SUBRANGE(
                    std::ranges::begin(r), std::ranges::end(r))};
            return parser_interface(
                token_parser<token_spec, decltype(expected)>(expected));
        }

        Expected expected_;
    };

#endif

    /** TODO */
    template<
        ctll::fixed_string Regex,
        auto ID,
        typename ValueType = string_view_tag,
        int Base = 10>
    constexpr parser_interface token_spec{
        token_parser<token_spec_t<Regex, ID, ValueType, Base>, detail::nope>()};

#ifndef BOOST_PARSER_DOXYGEN

    template<
        typename CharType,
        typename ID,
        ctll::fixed_string WsStr,
        ctll::fixed_string RegexStr,
        detail::nttp_array IDs,
        detail::nttp_array Specs>
    template<
        ctll::fixed_string RegexStr2,
        auto ID2,
        typename ValueType,
        int Base>
    constexpr auto
    lexer_t<CharType, ID, WsStr, RegexStr, IDs, Specs>::operator|(
        parser_interface<token_parser<
            token_spec_t<RegexStr2, ID2, ValueType, Base>,
            detail::nope>> const &) const
    {
        static_assert(
            std::same_as<ID, decltype(ID2)>,
            "All id_types must be the same for all token_specs.");
        constexpr auto new_regex =
            detail::wrap_escape_concat<regex_str, RegexStr2>();
        constexpr auto new_ids = IDs.template append<(int)ID2>();
        constexpr auto new_specs = Specs.template append<detail::parse_spec_for<
            token_spec_t<RegexStr2, ID2, ValueType, Base>>()>();
        return lexer_t<CharType, ID, WsStr, new_regex, new_ids, new_specs>{};
    }


#endif

}}

#endif
