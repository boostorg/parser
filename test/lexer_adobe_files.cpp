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

enum class adobe_tokens {
    keyword_true_false,
    keyword_empty,
    identifier,
    lead_comment,
    trail_comment,
    quoted_string,
    number,
    eq_op,
    rel_op,
    mul_op,
    define,
    or_,
    and_
};

int main()
{
    {
        // Document that maximum munch does not appear to apply -- putting "<=="
        // after "<|>|<=|>=" causes input "<==" to be tokenized as "<", "==".
        auto const lexer =
            bp::lexer<char, adobe_tokens> |

            bp::token_spec<
                "true|false",
                adobe_tokens::keyword_true_false,
                bool> |
            bp::token_spec<"empty", adobe_tokens::keyword_empty> |
            bp::token_spec<"[a-zA-Z]\\w*", adobe_tokens::identifier> |
            bp::token_spec<
                "\\/\\*[^*]*\\*+(?:[^/*][^*]*\\*+)*\\/",
                adobe_tokens::lead_comment> |
            bp::token_spec<"\\/\\/.*$", adobe_tokens::trail_comment> |
            bp::token_spec<
                "\\\"[^\\\"]*\\\"|'[^']*'",
                adobe_tokens::quoted_string> |
            bp::token_spec<"\\d+(?:\\.\\d*)?", adobe_tokens::number, double> |
            bp::token_spec<"==|!=", adobe_tokens::eq_op> |
            bp::token_spec<"<==", adobe_tokens::define> |
            bp::token_spec<"<|>|<=|>=", adobe_tokens::rel_op> |
            bp::token_spec<"\\*|\\/|%", adobe_tokens::mul_op> |
            bp::token_spec<"\\|\\|", adobe_tokens::or_> |
            bp::token_spec<"&&", adobe_tokens::and_> |
            bp::token_chars<
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

#if 0
        std::cout << "lexer.regex_str =\n"
                  << (lexer.regex_str.content | bp::as_utf8) << "\n";
        std::cout << "lexer.ws_str =\n"
                  << (lexer.ws_str.content | bp::as_utf8) << "\n";
        std::cout << "lexer::size()=" << lexer.size() << "\n";
        constexpr auto combined =
            bp::detail::wrap_escape_concat<lexer.regex_str, lexer.ws_str>();
        std::cout << "lexer combined regex str =\n"
                  << (combined.content | bp::as_utf8) << "\n";
        std::cout << "lexer IDs =\n" << lexer.ids() << "\n";
        std::cout << "lexer parse_specs =\n" << lexer.specs() << "\n";
#endif

        static_assert(decltype(lexer)::size() == 29 + 1);
        static_assert(std::same_as<decltype(lexer)::id_type, adobe_tokens>);

        // tokens_view from lexer
        {
            char const input[] = R"(/*
    Copyright 2005-2007 Adobe Systems Incorporated
    Distributed under the MIT License (see accompanying file LICENSE_1_0_0.txt
    or a copy at http://stlab.adobe.com/licenses.html)
*/

sheet alert_dialog
{
output:
    result <== { dummy_value: 42 };
})";
            // first, just make a ctre range
            {
                std::string_view const expected[] = {
                    R"(/*
    Copyright 2005-2007 Adobe Systems Incorporated
    Distributed under the MIT License (see accompanying file LICENSE_1_0_0.txt
    or a copy at http://stlab.adobe.com/licenses.html)
*/)",
                    R"(

)",
                    R"(sheet)",
                    R"( )",
                    R"(alert_dialog)",
                    R"(
)",
                    R"({)",
                    R"(
)",
                    R"(output)",
                    R"(:)",
                    R"(
    )",
                    R"(result)",
                    R"( )",
                    R"(<==)",
                    R"( )",
                    R"({)",
                    R"( )",
                    R"(dummy_value)",
                    R"(:)",
                    R"( )",
                    R"(42)",
                    R"( )",
                    R"(})",
                    R"(;)",
                    R"(
)",
                    R"(})"};
                auto r = lexer.regex_range(input);
                int position = 0;
                for (auto subrange : r) {
                    std::string_view sv = subrange;
                    BOOST_TEST(sv == expected[position]);
                    ++position;
                }
                BOOST_TEST(position == (int)std::size(expected));
                std::cout << "\n";
            }

            using tok_t = bp::token<char>;
            tok_t const expected[] = {
                tok_t((int)adobe_tokens::lead_comment, R"(/*
    Copyright 2005-2007 Adobe Systems Incorporated
    Distributed under the MIT License (see accompanying file LICENSE_1_0_0.txt
    or a copy at http://stlab.adobe.com/licenses.html)
*/)"),
                tok_t((int)adobe_tokens::identifier, "sheet"),
                tok_t((int)adobe_tokens::identifier, "alert_dialog"),
                tok_t(bp::character_id, (long long)'{'),
                tok_t((int)adobe_tokens::identifier, "output"),
                tok_t(bp::character_id, (long long)':'),
                tok_t((int)adobe_tokens::identifier, "result"),
                tok_t((int)adobe_tokens::define, "<=="),
                tok_t(bp::character_id, (long long)'{'),
                tok_t((int)adobe_tokens::identifier, "dummy_value"),
                tok_t(bp::character_id, (long long)':'),
                tok_t((int)adobe_tokens::number, (long double)42.0),
                tok_t(bp::character_id, (long long)'}'),
                tok_t(bp::character_id, (long long)';'),
                tok_t(bp::character_id, (long long)'}')};

            // make a tokens_view
            {
                auto r = bp::tokens_view(input, lexer);
                int position = 0;
                for (auto tok : r) {
                    BOOST_TEST(tok == expected[position]);
                    ++position;
                }
                BOOST_TEST(position == (int)std::size(expected));
            }

            // to_tokens range adaptor
            {
                int position = 0;
                for (auto tok: bp::to_tokens(input, lexer)) {
                    BOOST_TEST(tok == expected[position]);
                    ++position;
                }
                BOOST_TEST(position == (int)std::size(expected));
            }
            {
                std::string const input_str = input;
                int position = 0;
                for (auto tok: bp::to_tokens(input_str, lexer)) {
                    BOOST_TEST(tok == expected[position]);
                    ++position;
                }
                BOOST_TEST(position == (int)std::size(expected));
            }
            {
                int position = 0;
                for (auto tok : std::string(input) | bp::to_tokens(lexer)) {
                    BOOST_TEST(tok == expected[position]);
                    ++position;
                }
                BOOST_TEST(position == (int)std::size(expected));
            }

            // using external caches
            {
                std::vector<bp::token<char>> cache;
                int position = 0;
                for (auto tok : bp::to_tokens(input, lexer, std::ref(cache))) {
                    BOOST_TEST(tok == expected[position]);
                    ++position;
                }
                BOOST_TEST(position == (int)std::size(expected));
            }
            {
                boost::container::small_vector<bp::token<char>, 10> cache;
                int position = 0;
                for (auto tok : input | bp::to_tokens(lexer, std::ref(cache))) {
                    BOOST_TEST(tok == expected[position]);
                    ++position;
                }
                BOOST_TEST(position == (int)std::size(expected));
            }

            {
                char const large_input[] = R"(/*
    Copyright 2005-2007 Adobe Systems Incorporated
    Distributed under the MIT License (see accompanying file LICENSE_1_0_0.txt
    or a copy at http://stlab.adobe.com/licenses.html)
*/

sheet image_size
{
input:
    original_width          : 1600;
    original_height         : 1200;
    original_resolution     : 300;

constant:
    original_doc_width      : original_width / original_resolution;
    original_doc_height     : original_height / original_resolution;

interface:
    resample                : true;
    unlink  constrain       : true <== resample ? constrain : true;
    unlink  scale_styles    : true <== resample && constrain ? scale_styles : false;

    resample_method         : @bicubic;

    dim_width_pixels        : original_width <== resample ? round(dim_width_pixels) : original_width;
    dim_width_percent       : 100 <== resample ? dim_width_percent : 100;

    dim_height_pixels       : original_height <== resample ? round(dim_height_pixels) : original_height;
    dim_height_percent      : 100 <== resample ? dim_height_percent : 100;

    doc_width_inches        : original_doc_width;
    doc_width_percent       : 100;

    /*
        Resolution must be initialized before width and height inches to allow proportions
        to be constrained.
    */
    doc_resolution          : original_resolution;
    
    doc_height_inches       : original_doc_height;
    doc_height_percent      : 100;

    auto_quality            : @draft;
    
    screen_lpi; // initialized from doc_resolution

logic:
    relate {
        doc_width_inches    <== doc_width_percent * original_doc_width / 100;
        doc_width_percent   <== doc_width_inches * 100 / original_doc_width;
    }

    relate {
        doc_height_inches   <== doc_height_percent * original_doc_height / 100;
        doc_height_percent  <== doc_height_inches * 100 / original_doc_height;
    }

    relate {
        screen_lpi      <== doc_resolution / (auto_quality == @draft ? 1 : (auto_quality == @good ? 1.5 : 2.0));
        doc_resolution  <== screen_lpi * (auto_quality == @draft ? 1 : (auto_quality == @good ? 1.5 : 2.0));
    }

    when (resample) relate {
        dim_width_pixels    <== dim_width_percent * original_width / 100;
        dim_width_percent   <== dim_width_pixels * 100 / original_width;
    }

    when (resample) relate {
        dim_height_pixels   <== dim_height_percent * original_height / 100;
        dim_height_percent  <== dim_height_pixels * 100 / original_height;
    }

    when (resample) relate {
        doc_width_inches    <== dim_width_pixels / doc_resolution;
        dim_width_pixels    <== doc_width_inches * doc_resolution;
        doc_resolution      <== dim_width_pixels / doc_width_inches;
    }

    when (resample) relate {
        doc_height_inches   <== dim_height_pixels / doc_resolution;
        dim_height_pixels   <== doc_height_inches * doc_resolution;
        doc_resolution      <== dim_height_pixels / doc_height_inches;
    }

    when (!resample) relate {
        doc_resolution      <== original_width / doc_width_inches;
        doc_width_inches    <== original_width / doc_resolution;
    }

    when (!resample) relate {
        doc_resolution      <== original_height / doc_height_inches;
        doc_height_inches   <== original_height / doc_resolution;
    }

    when (constrain && resample) relate {
        dim_width_percent       <== dim_height_percent;
        dim_height_percent      <== dim_width_percent;
    }

 output:
    byte_count      <== dim_width_pixels * dim_height_pixels * 32;

    result          <== resample ?  {
                                        command:            @resize_image,
                                        width:              dim_width_pixels,
                                        height:             dim_height_pixels,
                                        resolution:         doc_resolution,
                                        scale_styles:       scale_styles,
                                        resample_method:    resample_method
                                    } : {
                                        command:            @set_resolution,
                                        resolution:         doc_resolution
                                    };

 invariant:
    width_max       <== dim_width_pixels <= 300000;
    height_max      <== dim_height_pixels <= 300000;
}
)";

                tok_t const expected[] = {
                    tok_t((int)adobe_tokens::lead_comment, R"(/*
    Copyright 2005-2007 Adobe Systems Incorporated
    Distributed under the MIT License (see accompanying file LICENSE_1_0_0.txt
    or a copy at http://stlab.adobe.com/licenses.html)
*/)"),
                    tok_t((int)adobe_tokens::identifier, "sheet"),
                    tok_t((int)adobe_tokens::identifier, "image_size"),
                    tok_t((int)bp::character_id, (long long)'{'),
                    tok_t((int)adobe_tokens::identifier, "input"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, "original_width"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::number, (long double)1600.0),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "original_height"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::number, (long double)1200.0),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "original_resolution"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::number, (long double)300.0),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "constant"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, "original_doc_width"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, "original_width"),
                    tok_t((int)adobe_tokens::mul_op, "/"),
                    tok_t((int)adobe_tokens::identifier, "original_resolution"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "original_doc_height"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, "original_height"),
                    tok_t((int)adobe_tokens::mul_op, "/"),
                    tok_t((int)adobe_tokens::identifier, "original_resolution"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "interface"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, "resample"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::keyword_true_false, 1ll),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "unlink"),
                    tok_t((int)adobe_tokens::identifier, "constrain"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::keyword_true_false, 1ll),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "resample"),
                    tok_t((int)bp::character_id, (long long)'?'),
                    tok_t((int)adobe_tokens::identifier, "constrain"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::keyword_true_false, 1ll),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "unlink"),
                    tok_t((int)adobe_tokens::identifier, "scale_styles"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::keyword_true_false, 1ll),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "resample"),
                    tok_t((int)adobe_tokens::and_, "&&"),
                    tok_t((int)adobe_tokens::identifier, "constrain"),
                    tok_t((int)bp::character_id, (long long)'?'),
                    tok_t((int)adobe_tokens::identifier, "scale_styles"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::keyword_true_false, 0ll),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "resample_method"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)bp::character_id, (long long)'@'),
                    tok_t((int)adobe_tokens::identifier, "bicubic"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "dim_width_pixels"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, "original_width"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "resample"),
                    tok_t((int)bp::character_id, (long long)'?'),
                    tok_t((int)adobe_tokens::identifier, "round"),
                    tok_t((int)bp::character_id, (long long)'('),
                    tok_t((int)adobe_tokens::identifier, "dim_width_pixels"),
                    tok_t((int)bp::character_id, (long long)')'),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, "original_width"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "dim_width_percent"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::number, (long double)100.0),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "resample"),
                    tok_t((int)bp::character_id, (long long)'?'),
                    tok_t((int)adobe_tokens::identifier, "dim_width_percent"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::number, (long double)100.0),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "dim_height_pixels"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, "original_height"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "resample"),
                    tok_t((int)bp::character_id, (long long)'?'),
                    tok_t((int)adobe_tokens::identifier, "round"),
                    tok_t((int)bp::character_id, (long long)'('),
                    tok_t((int)adobe_tokens::identifier, "dim_height_pixels"),
                    tok_t((int)bp::character_id, (long long)')'),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, "original_height"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "dim_height_percent"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::number, (long double)100.0),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "resample"),
                    tok_t((int)bp::character_id, (long long)'?'),
                    tok_t((int)adobe_tokens::identifier, "dim_height_percent"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::number, (long double)100.0),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "doc_width_inches"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, "original_doc_width"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "doc_width_percent"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::number, (long double)100.0),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::lead_comment, R"(/*
        Resolution must be initialized before width and height inches to allow proportions
        to be constrained.
    */)"),
                    tok_t((int)adobe_tokens::identifier, "doc_resolution"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, "original_resolution"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "doc_height_inches"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, "original_doc_height"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "doc_height_percent"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::number, (long double)100.0),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "auto_quality"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)bp::character_id, (long long)'@'),
                    tok_t((int)adobe_tokens::identifier, "draft"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "screen_lpi"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t(
                        (int)adobe_tokens::trail_comment,
                        "// initialized from doc_resolution"),
                    tok_t((int)adobe_tokens::identifier, "logic"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, "relate"),
                    tok_t((int)bp::character_id, (long long)'{'),
                    tok_t((int)adobe_tokens::identifier, "doc_width_inches"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "doc_width_percent"),
                    tok_t((int)adobe_tokens::mul_op, "*"),
                    tok_t((int)adobe_tokens::identifier, "original_doc_width"),
                    tok_t((int)adobe_tokens::mul_op, "/"),
                    tok_t((int)adobe_tokens::number, (long double)100.0),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "doc_width_percent"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "doc_width_inches"),
                    tok_t((int)adobe_tokens::mul_op, "*"),
                    tok_t((int)adobe_tokens::number, (long double)100.0),
                    tok_t((int)adobe_tokens::mul_op, "/"),
                    tok_t((int)adobe_tokens::identifier, "original_doc_width"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)bp::character_id, (long long)'}'),
                    tok_t((int)adobe_tokens::identifier, "relate"),
                    tok_t((int)bp::character_id, (long long)'{'),
                    tok_t((int)adobe_tokens::identifier, "doc_height_inches"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "doc_height_percent"),
                    tok_t((int)adobe_tokens::mul_op, "*"),
                    tok_t((int)adobe_tokens::identifier, "original_doc_height"),
                    tok_t((int)adobe_tokens::mul_op, "/"),
                    tok_t((int)adobe_tokens::number, (long double)100.0),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "doc_height_percent"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "doc_height_inches"),
                    tok_t((int)adobe_tokens::mul_op, "*"),
                    tok_t((int)adobe_tokens::number, (long double)100.0),
                    tok_t((int)adobe_tokens::mul_op, "/"),
                    tok_t((int)adobe_tokens::identifier, "original_doc_height"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)bp::character_id, (long long)'}'),
                    tok_t((int)adobe_tokens::identifier, "relate"),
                    tok_t((int)bp::character_id, (long long)'{'),
                    tok_t((int)adobe_tokens::identifier, "screen_lpi"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "doc_resolution"),
                    tok_t((int)adobe_tokens::mul_op, "/"),
                    tok_t((int)bp::character_id, (long long)'('),
                    tok_t((int)adobe_tokens::identifier, "auto_quality"),
                    tok_t((int)adobe_tokens::eq_op, "=="),
                    tok_t((int)bp::character_id, (long long)'@'),
                    tok_t((int)adobe_tokens::identifier, "draft"),
                    tok_t((int)bp::character_id, (long long)'?'),
                    tok_t((int)adobe_tokens::number, (long double)1.0),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)bp::character_id, (long long)'('),
                    tok_t((int)adobe_tokens::identifier, "auto_quality"),
                    tok_t((int)adobe_tokens::eq_op, "=="),
                    tok_t((int)bp::character_id, (long long)'@'),
                    tok_t((int)adobe_tokens::identifier, "good"),
                    tok_t((int)bp::character_id, (long long)'?'),
                    tok_t((int)adobe_tokens::number, (long double)1.5),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::number, (long double)2.0),
                    tok_t((int)bp::character_id, (long long)')'),
                    tok_t((int)bp::character_id, (long long)')'),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "doc_resolution"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "screen_lpi"),
                    tok_t((int)adobe_tokens::mul_op, "*"),
                    tok_t((int)bp::character_id, (long long)'('),
                    tok_t((int)adobe_tokens::identifier, "auto_quality"),
                    tok_t((int)adobe_tokens::eq_op, "=="),
                    tok_t((int)bp::character_id, (long long)'@'),
                    tok_t((int)adobe_tokens::identifier, "draft"),
                    tok_t((int)bp::character_id, (long long)'?'),
                    tok_t((int)adobe_tokens::number, (long double)1.0),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)bp::character_id, (long long)'('),
                    tok_t((int)adobe_tokens::identifier, "auto_quality"),
                    tok_t((int)adobe_tokens::eq_op, "=="),
                    tok_t((int)bp::character_id, (long long)'@'),
                    tok_t((int)adobe_tokens::identifier, "good"),
                    tok_t((int)bp::character_id, (long long)'?'),
                    tok_t((int)adobe_tokens::number, (long double)1.5),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::number, (long double)2.0),
                    tok_t((int)bp::character_id, (long long)')'),
                    tok_t((int)bp::character_id, (long long)')'),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)bp::character_id, (long long)'}'),
                    tok_t((int)adobe_tokens::identifier, "when"),
                    tok_t((int)bp::character_id, (long long)'('),
                    tok_t((int)adobe_tokens::identifier, "resample"),
                    tok_t((int)bp::character_id, (long long)')'),
                    tok_t((int)adobe_tokens::identifier, "relate"),
                    tok_t((int)bp::character_id, (long long)'{'),
                    tok_t((int)adobe_tokens::identifier, "dim_width_pixels"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "dim_width_percent"),
                    tok_t((int)adobe_tokens::mul_op, "*"),
                    tok_t((int)adobe_tokens::identifier, "original_width"),
                    tok_t((int)adobe_tokens::mul_op, "/"),
                    tok_t((int)adobe_tokens::number, (long double)100.0),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "dim_width_percent"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "dim_width_pixels"),
                    tok_t((int)adobe_tokens::mul_op, "*"),
                    tok_t((int)adobe_tokens::number, (long double)100.0),
                    tok_t((int)adobe_tokens::mul_op, "/"),
                    tok_t((int)adobe_tokens::identifier, "original_width"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)bp::character_id, (long long)'}'),
                    tok_t((int)adobe_tokens::identifier, "when"),
                    tok_t((int)bp::character_id, (long long)'('),
                    tok_t((int)adobe_tokens::identifier, "resample"),
                    tok_t((int)bp::character_id, (long long)')'),
                    tok_t((int)adobe_tokens::identifier, "relate"),
                    tok_t((int)bp::character_id, (long long)'{'),
                    tok_t((int)adobe_tokens::identifier, "dim_height_pixels"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "dim_height_percent"),
                    tok_t((int)adobe_tokens::mul_op, "*"),
                    tok_t((int)adobe_tokens::identifier, "original_height"),
                    tok_t((int)adobe_tokens::mul_op, "/"),
                    tok_t((int)adobe_tokens::number, (long double)100.0),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "dim_height_percent"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "dim_height_pixels"),
                    tok_t((int)adobe_tokens::mul_op, "*"),
                    tok_t((int)adobe_tokens::number, (long double)100.0),
                    tok_t((int)adobe_tokens::mul_op, "/"),
                    tok_t((int)adobe_tokens::identifier, "original_height"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)bp::character_id, (long long)'}'),
                    tok_t((int)adobe_tokens::identifier, "when"),
                    tok_t((int)bp::character_id, (long long)'('),
                    tok_t((int)adobe_tokens::identifier, "resample"),
                    tok_t((int)bp::character_id, (long long)')'),
                    tok_t((int)adobe_tokens::identifier, "relate"),
                    tok_t((int)bp::character_id, (long long)'{'),
                    tok_t((int)adobe_tokens::identifier, "doc_width_inches"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "dim_width_pixels"),
                    tok_t((int)adobe_tokens::mul_op, "/"),
                    tok_t((int)adobe_tokens::identifier, "doc_resolution"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "dim_width_pixels"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "doc_width_inches"),
                    tok_t((int)adobe_tokens::mul_op, "*"),
                    tok_t((int)adobe_tokens::identifier, "doc_resolution"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "doc_resolution"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "dim_width_pixels"),
                    tok_t((int)adobe_tokens::mul_op, "/"),
                    tok_t((int)adobe_tokens::identifier, "doc_width_inches"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)bp::character_id, (long long)'}'),
                    tok_t((int)adobe_tokens::identifier, "when"),
                    tok_t((int)bp::character_id, (long long)'('),
                    tok_t((int)adobe_tokens::identifier, "resample"),
                    tok_t((int)bp::character_id, (long long)')'),
                    tok_t((int)adobe_tokens::identifier, "relate"),
                    tok_t((int)bp::character_id, (long long)'{'),
                    tok_t((int)adobe_tokens::identifier, "doc_height_inches"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "dim_height_pixels"),
                    tok_t((int)adobe_tokens::mul_op, "/"),
                    tok_t((int)adobe_tokens::identifier, "doc_resolution"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "dim_height_pixels"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "doc_height_inches"),
                    tok_t((int)adobe_tokens::mul_op, "*"),
                    tok_t((int)adobe_tokens::identifier, "doc_resolution"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "doc_resolution"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "dim_height_pixels"),
                    tok_t((int)adobe_tokens::mul_op, "/"),
                    tok_t((int)adobe_tokens::identifier, "doc_height_inches"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)bp::character_id, (long long)'}'),
                    tok_t((int)adobe_tokens::identifier, "when"),
                    tok_t((int)bp::character_id, (long long)'('),
                    tok_t((int)bp::character_id, (long long)'!'),
                    tok_t((int)adobe_tokens::identifier, "resample"),
                    tok_t((int)bp::character_id, (long long)')'),
                    tok_t((int)adobe_tokens::identifier, "relate"),
                    tok_t((int)bp::character_id, (long long)'{'),
                    tok_t((int)adobe_tokens::identifier, "doc_resolution"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "original_width"),
                    tok_t((int)adobe_tokens::mul_op, "/"),
                    tok_t((int)adobe_tokens::identifier, "doc_width_inches"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "doc_width_inches"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "original_width"),
                    tok_t((int)adobe_tokens::mul_op, "/"),
                    tok_t((int)adobe_tokens::identifier, "doc_resolution"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)bp::character_id, (long long)'}'),
                    tok_t((int)adobe_tokens::identifier, "when"),
                    tok_t((int)bp::character_id, (long long)'('),
                    tok_t((int)bp::character_id, (long long)'!'),
                    tok_t((int)adobe_tokens::identifier, "resample"),
                    tok_t((int)bp::character_id, (long long)')'),
                    tok_t((int)adobe_tokens::identifier, "relate"),
                    tok_t((int)bp::character_id, (long long)'{'),
                    tok_t((int)adobe_tokens::identifier, "doc_resolution"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "original_height"),
                    tok_t((int)adobe_tokens::mul_op, "/"),
                    tok_t((int)adobe_tokens::identifier, "doc_height_inches"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "doc_height_inches"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "original_height"),
                    tok_t((int)adobe_tokens::mul_op, "/"),
                    tok_t((int)adobe_tokens::identifier, "doc_resolution"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)bp::character_id, (long long)'}'),
                    tok_t((int)adobe_tokens::identifier, "when"),
                    tok_t((int)bp::character_id, (long long)'('),
                    tok_t((int)adobe_tokens::identifier, "constrain"),
                    tok_t((int)adobe_tokens::and_, "&&"),
                    tok_t((int)adobe_tokens::identifier, "resample"),
                    tok_t((int)bp::character_id, (long long)')'),
                    tok_t((int)adobe_tokens::identifier, "relate"),
                    tok_t((int)bp::character_id, (long long)'{'),
                    tok_t((int)adobe_tokens::identifier, "dim_width_percent"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "dim_height_percent"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "dim_height_percent"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "dim_width_percent"),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)bp::character_id, (long long)'}'),
                    tok_t((int)adobe_tokens::identifier, "output"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, "byte_count"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "dim_width_pixels"),
                    tok_t((int)adobe_tokens::mul_op, "*"),
                    tok_t((int)adobe_tokens::identifier, "dim_height_pixels"),
                    tok_t((int)adobe_tokens::mul_op, "*"),
                    tok_t((int)adobe_tokens::number, (long double)32.0),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "result"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "resample"),
                    tok_t((int)bp::character_id, (long long)'?'),
                    tok_t((int)bp::character_id, (long long)'{'),
                    tok_t((int)adobe_tokens::identifier, "command"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)bp::character_id, (long long)'@'),
                    tok_t((int)adobe_tokens::identifier, "resize_image"),
                    tok_t((int)bp::character_id, (long long)','),
                    tok_t((int)adobe_tokens::identifier, "width"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, "dim_width_pixels"),
                    tok_t((int)bp::character_id, (long long)','),
                    tok_t((int)adobe_tokens::identifier, "height"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, "dim_height_pixels"),
                    tok_t((int)bp::character_id, (long long)','),
                    tok_t((int)adobe_tokens::identifier, "resolution"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, "doc_resolution"),
                    tok_t((int)bp::character_id, (long long)','),
                    tok_t((int)adobe_tokens::identifier, "scale_styles"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, "scale_styles"),
                    tok_t((int)bp::character_id, (long long)','),
                    tok_t((int)adobe_tokens::identifier, "resample_method"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, "resample_method"),
                    tok_t((int)bp::character_id, (long long)'}'),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)bp::character_id, (long long)'{'),
                    tok_t((int)adobe_tokens::identifier, "command"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)bp::character_id, (long long)'@'),
                    tok_t((int)adobe_tokens::identifier, "set_resolution"),
                    tok_t((int)bp::character_id, (long long)','),
                    tok_t((int)adobe_tokens::identifier, "resolution"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, "doc_resolution"),
                    tok_t((int)bp::character_id, (long long)'}'),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "invariant"),
                    tok_t((int)bp::character_id, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, "width_max"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "dim_width_pixels"),
                    tok_t((int)adobe_tokens::rel_op, "<"),
                    tok_t((int)bp::character_id, (long long)'='),
                    tok_t((int)adobe_tokens::number, (long double)300000.0),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, "height_max"),
                    tok_t((int)adobe_tokens::define, "<=="),
                    tok_t((int)adobe_tokens::identifier, "dim_height_pixels"),
                    tok_t((int)adobe_tokens::rel_op, "<"),
                    tok_t((int)bp::character_id, (long long)'='),
                    tok_t((int)adobe_tokens::number, (long double)300000.0),
                    tok_t((int)bp::character_id, (long long)';'),
                    tok_t((int)bp::character_id, (long long)'}')};

                int position = 0;
                for (auto tok :
                     std::string(large_input) | bp::to_tokens(lexer)) {
                    BOOST_TEST(tok == expected[position]);
                    if (tok != expected[position]) {
                        std::cout << "At pos=" << position << ": got " << tok
                                  << " expected " << expected[position] << "\n";
                    }
                    ++position;
                }
                BOOST_TEST(position == (int)std::size(expected));
            }
       }
    }

    return boost::report_errors();
}
