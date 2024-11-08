/**
 *   Copyright (C) 2024 T. Zachary Laine
 *
 *   Distributed under the Boost Software License, Version 1.0. (See
 *   accompanying file LICENSE_1_0.txt or copy at
 *   http://www.boost.org/LICENSE_1_0.txt)
 */
#define BOOST_PARSER_TESTING
#include <boost/parser/lexer.hpp>

#include <boost/parser/transcode_view.hpp>

#include "ill_formed.hpp"

#include <boost/core/lightweight_test.hpp>
#include <boost/container/small_vector.hpp>

#include <deque>


namespace bp = boost::parser;

enum class my_tokens { foo, bar, baz };

int main()
{
#if BOOST_PARSER_USE_CONCEPTS

    // formation of token_specs
    {
        auto const token_spec = bp::token_spec<"foo", 12>;

        const bp::token_spec_t<"foo", 12, bp::none, 10> token_spec_explicit;
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo", my_tokens::foo>;

        const bp::token_spec_t<"foo", my_tokens::foo, bp::none, 10>
            token_spec_explicit;
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"bar", my_tokens::bar>;

        const bp::token_spec_t<"bar", my_tokens::bar, bp::none, 10>
            token_spec_explicit;
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo", 12, int, 2>;

        const bp::token_spec_t<"foo", 12, int, 2> token_spec_explicit;
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo", 12>;

        const bp::token_spec_t<"foo", 12, bp::none, 10> token_spec_explicit;
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo", 12, unsigned int, 8>;

        const bp::token_spec_t<"foo", 12, unsigned int, 8> token_spec_explicit;
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo", 12, short>;

        const bp::token_spec_t<"foo", 12, short, 10> token_spec_explicit;
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo", 12, float>;

        const bp::token_spec_t<"foo", 12, float, 10> token_spec_explicit;
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo", 12, double>;

        const bp::token_spec_t<"foo", 12, double, 10> token_spec_explicit;
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }

    // making lexers
    {
        auto const lexer = bp::lexer<char, my_tokens> |
                           bp::token_spec<"foo", my_tokens::foo> |
                           bp::token_spec<"bar", my_tokens::bar> |
                           bp::token_spec<"baz", my_tokens::baz>;

        // +1 because of the 0-group
        static_assert(decltype(lexer)::size() == 3 + 1);
        static_assert(std::same_as<decltype(lexer)::id_type, my_tokens>);
    }
    {
        auto const lexer = bp::lexer<char, my_tokens> | bp::token_chars<'='>;

        static_assert(decltype(lexer)::size() == 1 + 1);
        static_assert(std::same_as<decltype(lexer)::id_type, my_tokens>);
    }
    {
        auto const lexer = bp::lexer<char, my_tokens> | bp::token_chars<'='> |
                           bp::token_spec<"foo", my_tokens::foo> |
                           bp::token_spec<"bar", my_tokens::bar> |
                           bp::token_spec<"baz", my_tokens::baz>;

        static_assert(decltype(lexer)::size() == 4 + 1);
        static_assert(std::same_as<decltype(lexer)::id_type, my_tokens>);
    }
    {
        auto const lexer =
            bp::lexer<char, my_tokens> | bp::token_spec<"foo", my_tokens::foo> |
            bp::token_spec<"bar", my_tokens::bar> |
            bp::token_spec<"baz", my_tokens::baz> | bp::token_chars<'='>;

        static_assert(decltype(lexer)::size() == 4 + 1);
        static_assert(std::same_as<decltype(lexer)::id_type, my_tokens>);
    }
    {
        auto const lexer = bp::lexer<char, my_tokens> | bp::token_chars<
                                                            '=',
                                                            '+',
                                                            '-',
                                                            '!',
                                                            '?',
                                                            ':',
                                                            '.',
                                                            ',',
                                                            '(',
                                                            ')',
                                                            '[',
                                                            ']',
                                                            '{',
                                                            '}',
                                                            '@',
                                                            ';'>;

        static_assert(decltype(lexer)::size() == 16 + 1);
        static_assert(std::same_as<decltype(lexer)::id_type, my_tokens>);
    }
#if 0 // This is a test of whether the escapes work for every possible char
      // value accepted by detail::token_chars_spec.  This takes a long time and
      // really only needs to happen once.
    {
        auto const lexer = bp::lexer<char, my_tokens> | bp::token_chars<
                                                              char(0),
                                                              char(1),
                                                              char(2),
                                                              char(3),
                                                              char(4),
                                                              char(5),
                                                              char(6),
                                                              char(7),
                                                              char(8),
                                                              char(9),
                                                              char(10),
                                                              char(11),
                                                              char(12),
                                                              char(13),
                                                              char(14),
                                                              char(15),
                                                              char(16),
                                                              char(17),
                                                              char(18),
                                                              char(19),
                                                              char(20),
                                                              char(21),
                                                              char(22),
                                                              char(23),
                                                              char(24),
                                                              char(25),
                                                              char(26),
                                                              char(27),
                                                              char(28),
                                                              char(29),
                                                              char(30),
                                                              char(31),
                                                              char(32),
                                                              char(33),
                                                              char(34),
                                                              char(35),
                                                              char(36),
                                                              char(37),
                                                              char(38),
                                                              char(39),
                                                              char(40),
                                                              char(41),
                                                              char(42),
                                                              char(43),
                                                              char(44),
                                                              char(45),
                                                              char(46),
                                                              char(47),
                                                              char(48),
                                                              char(49),
                                                              char(50),
                                                              char(51),
                                                              char(52),
                                                              char(53),
                                                              char(54),
                                                              char(55),
                                                              char(56),
                                                              char(57),
                                                              char(58),
                                                              char(59),
                                                              char(60),
                                                              char(61),
                                                              char(62),
                                                              char(63),
                                                              char(64),
                                                              char(65),
                                                              char(66),
                                                              char(67),
                                                              char(68),
                                                              char(69),
                                                              char(70),
                                                              char(71),
                                                              char(72),
                                                              char(73),
                                                              char(74),
                                                              char(75),
                                                              char(76),
                                                              char(77),
                                                              char(78),
                                                              char(79),
                                                              char(80),
                                                              char(81),
                                                              char(82),
                                                              char(83),
                                                              char(84),
                                                              char(85),
                                                              char(86),
                                                              char(87),
                                                              char(88),
                                                              char(89),
                                                              char(90),
                                                              char(91),
                                                              char(92),
                                                              char(93),
                                                              char(94),
                                                              char(95),
                                                              char(96),
                                                              char(97),
                                                              char(98),
                                                              char(99),

                                                              char(100),
                                                              char(101),
                                                              char(103),
                                                              char(102),
                                                              char(104),
                                                              char(105),
                                                              char(106),
                                                              char(107),
                                                              char(108),
                                                              char(109),
                                                              char(110),
                                                              char(111),
                                                              char(112),
                                                              char(113),
                                                              char(114),
                                                              char(115),
                                                              char(116),
                                                              char(117),
                                                              char(118),
                                                              char(119),
                                                              char(120),
                                                              char(121),
                                                              char(122),
                                                              char(123),
                                                              char(124),
                                                              char(125),
                                                              char(126),
                                                              char(127)>;
    }
#endif

    // TODO: Need tests with the various supported kinds of input sequence.

    // TODO: Test different UTF combinations (no envoding + no encoding), and
    // all combinations of (UTF-N token specs + UTF-M input).

    // TODO: Test const and mutable versions of tokens_view.

    // TODO: Add a lexing test for a lexer with no whitespace.

    // TODO: Document that every spec's chars need to be in the same UTF (or
    // none).  Wait -- is this actually true?  Tests needed....

#endif

    return boost::report_errors();
}
