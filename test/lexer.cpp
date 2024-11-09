/**
 *   Copyright (C) 2024 T. Zachary Laine
 *
 *   Distributed under the Boost Software License, Version 1.0. (See
 *   accompanying file LICENSE_1_0.txt or copy at
 *   http://www.boost.org/LICENSE_1_0.txt)
 */
#define BOOST_PARSER_TESTING
#include <boost/parser/lexer.hpp>
#include <boost/parser/parser.hpp>

#include <boost/parser/transcode_view.hpp>

#include "ill_formed.hpp"

#include <boost/core/lightweight_test.hpp>
#include <boost/container/small_vector.hpp>

#include <deque>


namespace bp = boost::parser;

enum class my_tokens { ws, foo, bar, baz };

int main()
{
    // formation of token_specs
    {
        auto const token_spec = bp::token_spec<"foo", 12>;

        bp::token_spec_t<"foo", 12, bp::string_view_tag, 10>
            token_spec_explicit;
        static_assert(std::same_as<
                      decltype(token_spec.parser_)::token_spec,
                      decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo", my_tokens::foo>;

        bp::token_spec_t<"foo", my_tokens::foo, bp::string_view_tag, 10>
            token_spec_explicit;
        static_assert(std::same_as<
                      decltype(token_spec.parser_)::token_spec,
                      decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"bar", my_tokens::bar>;

        bp::token_spec_t<"bar", my_tokens::bar, bp::string_view_tag, 10>
            token_spec_explicit;
        static_assert(std::same_as<
                      decltype(token_spec.parser_)::token_spec,
                      decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo", 12, int, 2>;

        bp::token_spec_t<"foo", 12, int, 2> token_spec_explicit;
        static_assert(std::same_as<
                      decltype(token_spec.parser_)::token_spec,
                      decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo", 12>;

        bp::token_spec_t<"foo", 12, bp::string_view_tag, 10>
            token_spec_explicit;
        static_assert(std::same_as<
                      decltype(token_spec.parser_)::token_spec,
                      decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo", 12, unsigned int, 8>;

        bp::token_spec_t<"foo", 12, unsigned int, 8> token_spec_explicit;
        static_assert(std::same_as<
                      decltype(token_spec.parser_)::token_spec,
                      decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo", 12, short>;

        bp::token_spec_t<"foo", 12, short, 10> token_spec_explicit;
        static_assert(std::same_as<
                      decltype(token_spec.parser_)::token_spec,
                      decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo", 12, float>;

        bp::token_spec_t<"foo", 12, float, 10> token_spec_explicit;
        static_assert(std::same_as<
                      decltype(token_spec.parser_)::token_spec,
                      decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo", 12, double>;

        bp::token_spec_t<"foo", 12, double, 10> token_spec_explicit;
        static_assert(std::same_as<
                      decltype(token_spec.parser_)::token_spec,
                      decltype(token_spec_explicit)>);
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

    {
        // Mixed UTFs.
        auto const lexer =
            bp::lexer<char, my_tokens> | bp::token_spec<"foo", my_tokens::foo> |
            bp::token_spec<u"bar", my_tokens::bar> |
            bp::token_spec<U"baz", my_tokens::baz> | bp::token_chars<'='>;

        // mutable vs. const token_views + mutable vs. const input views
        std::string input = "foo = bar";
        auto mr_mi = input | bp::to_tokens(lexer);
        auto const cr_mi = input | bp::to_tokens(lexer);

        auto const const_input = input;
        auto mr_ci = input | bp::to_tokens(lexer);
        auto const cr_ci = input | bp::to_tokens(lexer);

        using tok_t = bp::token<char>;
        tok_t const expected[] = {
            tok_t((int)my_tokens::foo, "foo"),
            tok_t(bp::character_id, (long long)'='),
            tok_t((int)my_tokens::bar, "bar")};

        int position = 0;

        position = 0;
        for (auto tok : mr_mi) {
            BOOST_TEST(tok == expected[position]);
            ++position;
        }
        BOOST_TEST(position == (int)std::size(expected));

        position = 0;
        for (auto tok : cr_mi) {
            BOOST_TEST(tok == expected[position]);
            ++position;
        }
        BOOST_TEST(position == (int)std::size(expected));

        position = 0;
        for (auto tok : mr_ci) {
            BOOST_TEST(tok == expected[position]);
            ++position;
        }
        BOOST_TEST(position == (int)std::size(expected));

        position = 0;
        for (auto tok : cr_ci) {
            BOOST_TEST(tok == expected[position]);
            ++position;
        }
        BOOST_TEST(position == (int)std::size(expected));
    }

    // Check basic plumbing of connecting UTF inputs to CTRE.
    {
        auto const lexer =
            bp::lexer<char, my_tokens> | bp::token_spec<"foo", my_tokens::foo> |
            bp::token_spec<"bar", my_tokens::bar> |
            bp::token_spec<"baz", my_tokens::baz> | bp::token_chars<'='>;

        std::string s = "foo = bar";
        using tok_t = bp::token<char>;
        tok_t const expected[] = {
            tok_t((int)my_tokens::foo, "foo"),
            tok_t(bp::character_id, (long long)'='),
            tok_t((int)my_tokens::bar, "bar")};

        auto const lexer8 = bp::lexer<char8_t, my_tokens> |
                            bp::token_spec<"foo", my_tokens::foo> |
                            bp::token_spec<"bar", my_tokens::bar> |
                            bp::token_spec<"baz", my_tokens::baz> |
                            bp::token_chars<'='>;

        std::u8string u8s = u8"foo = bar";
        using tok8_t = bp::token<char8_t>;
        tok8_t const expected8[] = {
            tok8_t((int)my_tokens::foo, u8"foo"),
            tok8_t(bp::character_id, (long long)'='),
            tok8_t((int)my_tokens::bar, u8"bar")};

        auto const lexer16 = bp::lexer<char16_t, my_tokens> |
                             bp::token_spec<"foo", my_tokens::foo> |
                             bp::token_spec<"bar", my_tokens::bar> |
                             bp::token_spec<"baz", my_tokens::baz> |
                             bp::token_chars<'='>;

        std::u16string u16s = u"foo = bar";
        using tok16_t = bp::token<char16_t>;
        tok16_t const expected16[] = {
            tok16_t((int)my_tokens::foo, u"foo"),
            tok16_t(bp::character_id, (long long)'='),
            tok16_t((int)my_tokens::bar, u"bar")};

        auto const lexer32 = bp::lexer<char32_t, my_tokens> |
                             bp::token_spec<"foo", my_tokens::foo> |
                             bp::token_spec<"bar", my_tokens::bar> |
                             bp::token_spec<"baz", my_tokens::baz> |
                             bp::token_chars<'='>;

        std::u32string u32s = U"foo = bar";
        using tok32_t = bp::token<char32_t>;
        tok32_t const expected32[] = {
            tok32_t((int)my_tokens::foo, U"foo"),
            tok32_t(bp::character_id, (long long)'='),
            tok32_t((int)my_tokens::bar, U"bar")};


        int position = 0;

        position = 0;
        for (auto tok : s | bp::to_tokens(lexer)) {
            BOOST_TEST(tok == expected[position]);
            static_assert(
                std::
                    same_as<decltype(tok.get_string_view()), std::string_view>);
            ++position;
        }
        BOOST_TEST(position == (int)std::size(expected));

        position = 0;
        for (auto tok : u8s | bp::to_tokens(lexer8)) {
            BOOST_TEST(tok == expected8[position]);
            static_assert(std::same_as<
                          decltype(tok.get_string_view()),
                          std::u8string_view>);
            ++position;
        }
        BOOST_TEST(position == (int)std::size(expected));

        position = 0;
        for (auto tok : u16s | bp::to_tokens(lexer16)) {
            BOOST_TEST(tok == expected16[position]);
            static_assert(std::same_as<
                          decltype(tok.get_string_view()),
                          std::u16string_view>);
            ++position;
        }
        BOOST_TEST(position == (int)std::size(expected));

        position = 0;
        for (auto tok : u32s | bp::to_tokens(lexer32)) {
            BOOST_TEST(tok == expected32[position]);
            static_assert(std::same_as<
                          decltype(tok.get_string_view()),
                          std::u32string_view>);
            ++position;
        }
        BOOST_TEST(position == (int)std::size(expected));
    }

    // no-ws lexer
    {
        auto const lexer = bp::lexer<char, my_tokens, bp::no_ws> |
                           bp::token_spec<"foo", my_tokens::foo> |
                           bp::token_spec<"bar", my_tokens::bar> |
                           bp::token_spec<"baz", my_tokens::baz> |
                           bp::token_chars<'='>;

        std::string s = "foo=bar";
        using tok_t = bp::token<char>;
        tok_t const expected[] = {
            tok_t((int)my_tokens::foo, "foo"),
            tok_t(bp::character_id, (long long)'='),
            tok_t((int)my_tokens::bar, "bar")};

        int position = 0;
        for (auto tok : s | bp::to_tokens(lexer)) {
            BOOST_TEST(tok == expected[position]);
            ++position;
        }
        BOOST_TEST(position == (int)std::size(expected));
    }

    // ws-as-token lexers
    {
        auto const lexer = bp::lexer<char, my_tokens, bp::no_ws> |
                           bp::token_spec<"\\s+", my_tokens::ws> |
                           bp::token_spec<"foo", my_tokens::foo> |
                           bp::token_spec<"bar", my_tokens::bar> |
                           bp::token_spec<"baz", my_tokens::baz> |
                           bp::token_chars<'='>;

        std::string s = "foo = bar";
        using tok_t = bp::token<char>;
        tok_t const expected[] = {
            tok_t((int)my_tokens::foo, "foo"),
            tok_t((int)my_tokens::ws, " "),
            tok_t(bp::character_id, (long long)'='),
            tok_t((int)my_tokens::ws, " "),
            tok_t((int)my_tokens::bar, "bar")};

        int position = 0;
        for (auto tok : s | bp::to_tokens(lexer)) {
            BOOST_TEST(tok == expected[position]);
            ++position;
        }
        BOOST_TEST(position == (int)std::size(expected));
    }

    // TODO: Document the limitation of CTRE that the input must be a
    // continguous_range, so that string_views can be formed.

    // TODO: Document that every spec's chars are assumed to be in UTF when
    // CTRE_STRING_IS_UTF8 is defined, and no encoding otherwise.  Also document
    // that char16_t is treated as UTF-16, but wchar_t and char32_t are *both*
    // treated as UTF-32, even on windows.

    return boost::report_errors();
}
