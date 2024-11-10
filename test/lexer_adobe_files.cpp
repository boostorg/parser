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
#include "adobe_lexer.hpp"

#include <boost/core/lightweight_test.hpp>
#include <boost/container/small_vector.hpp>

#include <deque>


namespace bp = boost::parser;

int main()
{
    {
        // TODO: Document that maximum munch does not appear to apply in CTRE
        // regexes -- putting "<==" after "<|>|<=|>=" causes input "<==" to be
        // tokenized as "<", "==".

        static_assert(decltype(adobe_lexer)::size() == 29 + 1);
        static_assert(
            std::same_as<decltype(adobe_lexer)::id_type, adobe_tokens>);

        // tokens_view from adobe_lexer
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

)",    R"(sheet)",  R"( )",  R"(alert_dialog)",
                    R"(
)",    R"({)",
                    R"(
)",    R"(output)", R"(:)",
                    R"(
    )",    R"(result)", R"( )",  R"(<==)",
                    R"( )", R"({)",      R"( )",  R"(dummy_value)",
                    R"(:)", R"( )",      R"(42)", R"( )",
                    R"(})", R"(;)",
                    R"(
)",    R"(})"};
                auto r = adobe_lexer.regex_range(input);
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
                tok_t((int)adobe_tokens::lead_comment, 0, R"(/*
    Copyright 2005-2007 Adobe Systems Incorporated
    Distributed under the MIT License (see accompanying file LICENSE_1_0_0.txt
    or a copy at http://stlab.adobe.com/licenses.html)
*/)"),
                tok_t((int)adobe_tokens::identifier, 0, "sheet"),
                tok_t((int)adobe_tokens::identifier, 0, "alert_dialog"),
                tok_t(bp::character_id, 0, (long long)'{'),
                tok_t((int)adobe_tokens::identifier, 0, "output"),
                tok_t(bp::character_id, 0, (long long)':'),
                tok_t((int)adobe_tokens::identifier, 0, "result"),
                tok_t((int)adobe_tokens::define, 0, "<=="),
                tok_t(bp::character_id, 0, (long long)'{'),
                tok_t((int)adobe_tokens::identifier, 0, "dummy_value"),
                tok_t(bp::character_id, 0, (long long)':'),
                tok_t((int)adobe_tokens::number, 0, (long double)42.0),
                tok_t(bp::character_id, 0, (long long)'}'),
                tok_t(bp::character_id, 0, (long long)';'),
                tok_t(bp::character_id, 0, (long long)'}')};

            // make a tokens_view
            {
                auto r = bp::tokens_view(input, adobe_lexer);
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
                for (auto tok : bp::to_tokens(input, adobe_lexer)) {
                    BOOST_TEST(tok == expected[position]);
                    ++position;
                }
                BOOST_TEST(position == (int)std::size(expected));
            }
            {
                std::string const input_str = input;
                int position = 0;
                for (auto tok : bp::to_tokens(input_str, adobe_lexer)) {
                    BOOST_TEST(tok == expected[position]);
                    ++position;
                }
                BOOST_TEST(position == (int)std::size(expected));
            }
            {
                int position = 0;
                for (auto tok :
                     std::string(input) | bp::to_tokens(adobe_lexer)) {
                    BOOST_TEST(tok == expected[position]);
                    ++position;
                }
                BOOST_TEST(position == (int)std::size(expected));
            }

            // using external caches
            {
                std::vector<bp::token<char>> cache;
                int position = 0;
                for (auto tok :
                     bp::to_tokens(input, adobe_lexer, std::ref(cache))) {
                    BOOST_TEST(tok == expected[position]);
                    ++position;
                }
                BOOST_TEST(position == (int)std::size(expected));
            }
            {
                boost::container::small_vector<bp::token<char>, 10> cache;
                int position = 0;
                for (auto tok :
                     input | bp::to_tokens(adobe_lexer, std::ref(cache))) {
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
                    tok_t((int)adobe_tokens::lead_comment, 0, R"(/*
    Copyright 2005-2007 Adobe Systems Incorporated
    Distributed under the MIT License (see accompanying file LICENSE_1_0_0.txt
    or a copy at http://stlab.adobe.com/licenses.html)
*/)"),
                    tok_t((int)adobe_tokens::identifier, 0, "sheet"),
                    tok_t((int)adobe_tokens::identifier, 0, "image_size"),
                    tok_t((int)bp::character_id, 0, (long long)'{'),
                    tok_t((int)adobe_tokens::identifier, 0, "input"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, 0, "original_width"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::number, 0, (long double)1600.0),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, 0, "original_height"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::number, 0, (long double)1200.0),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t(
                        (int)adobe_tokens::identifier,
                        0,
                        "original_resolution"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::number, 0, (long double)300.0),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, 0, "constant"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "original_doc_width"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, 0, "original_width"),
                    tok_t((int)adobe_tokens::mul_op, 0, "/"),
                    tok_t(
                        (int)adobe_tokens::identifier,
                        0,
                        "original_resolution"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t(
                        (int)adobe_tokens::identifier,
                        0,
                        "original_doc_height"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, 0, "original_height"),
                    tok_t((int)adobe_tokens::mul_op, 0, "/"),
                    tok_t(
                        (int)adobe_tokens::identifier,
                        0,
                        "original_resolution"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, 0, "interface"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, 0, "resample"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::keyword_true_false, 0, 1ll),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, 0, "unlink"),
                    tok_t((int)adobe_tokens::identifier, 0, "constrain"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::keyword_true_false, 0, 1ll),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t((int)adobe_tokens::identifier, 0, "resample"),
                    tok_t((int)bp::character_id, 0, (long long)'?'),
                    tok_t((int)adobe_tokens::identifier, 0, "constrain"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::keyword_true_false, 0, 1ll),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, 0, "unlink"),
                    tok_t((int)adobe_tokens::identifier, 0, "scale_styles"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::keyword_true_false, 0, 1ll),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t((int)adobe_tokens::identifier, 0, "resample"),
                    tok_t((int)adobe_tokens::and_, 0, "&&"),
                    tok_t((int)adobe_tokens::identifier, 0, "constrain"),
                    tok_t((int)bp::character_id, 0, (long long)'?'),
                    tok_t((int)adobe_tokens::identifier, 0, "scale_styles"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::keyword_true_false, 0, 0ll),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, 0, "resample_method"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)bp::character_id, 0, (long long)'@'),
                    tok_t((int)adobe_tokens::identifier, 0, "bicubic"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, 0, "dim_width_pixels"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, 0, "original_width"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t((int)adobe_tokens::identifier, 0, "resample"),
                    tok_t((int)bp::character_id, 0, (long long)'?'),
                    tok_t((int)adobe_tokens::identifier, 0, "round"),
                    tok_t((int)bp::character_id, 0, (long long)'('),
                    tok_t((int)adobe_tokens::identifier, 0, "dim_width_pixels"),
                    tok_t((int)bp::character_id, 0, (long long)')'),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, 0, "original_width"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "dim_width_percent"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::number, 0, (long double)100.0),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t((int)adobe_tokens::identifier, 0, "resample"),
                    tok_t((int)bp::character_id, 0, (long long)'?'),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "dim_width_percent"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::number, 0, (long double)100.0),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "dim_height_pixels"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, 0, "original_height"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t((int)adobe_tokens::identifier, 0, "resample"),
                    tok_t((int)bp::character_id, 0, (long long)'?'),
                    tok_t((int)adobe_tokens::identifier, 0, "round"),
                    tok_t((int)bp::character_id, 0, (long long)'('),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "dim_height_pixels"),
                    tok_t((int)bp::character_id, 0, (long long)')'),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, 0, "original_height"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "dim_height_percent"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::number, 0, (long double)100.0),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t((int)adobe_tokens::identifier, 0, "resample"),
                    tok_t((int)bp::character_id, 0, (long long)'?'),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "dim_height_percent"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::number, 0, (long double)100.0),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, 0, "doc_width_inches"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "original_doc_width"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "doc_width_percent"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::number, 0, (long double)100.0),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)adobe_tokens::lead_comment, 0, R"(/*
        Resolution must be initialized before width and height inches to allow proportions
        to be constrained.
    */)"),
                    tok_t((int)adobe_tokens::identifier, 0, "doc_resolution"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t(
                        (int)adobe_tokens::identifier,
                        0,
                        "original_resolution"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "doc_height_inches"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t(
                        (int)adobe_tokens::identifier,
                        0,
                        "original_doc_height"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "doc_height_percent"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::number, 0, (long double)100.0),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, 0, "auto_quality"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)bp::character_id, 0, (long long)'@'),
                    tok_t((int)adobe_tokens::identifier, 0, "draft"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, 0, "screen_lpi"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t(
                        (int)adobe_tokens::trail_comment,
                        0,
                        "// initialized from doc_resolution"),
                    tok_t((int)adobe_tokens::identifier, 0, "logic"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, 0, "relate"),
                    tok_t((int)bp::character_id, 0, (long long)'{'),
                    tok_t((int)adobe_tokens::identifier, 0, "doc_width_inches"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "doc_width_percent"),
                    tok_t((int)adobe_tokens::mul_op, 0, "*"),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "original_doc_width"),
                    tok_t((int)adobe_tokens::mul_op, 0, "/"),
                    tok_t((int)adobe_tokens::number, 0, (long double)100.0),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "doc_width_percent"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t((int)adobe_tokens::identifier, 0, "doc_width_inches"),
                    tok_t((int)adobe_tokens::mul_op, 0, "*"),
                    tok_t((int)adobe_tokens::number, 0, (long double)100.0),
                    tok_t((int)adobe_tokens::mul_op, 0, "/"),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "original_doc_width"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)bp::character_id, 0, (long long)'}'),
                    tok_t((int)adobe_tokens::identifier, 0, "relate"),
                    tok_t((int)bp::character_id, 0, (long long)'{'),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "doc_height_inches"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "doc_height_percent"),
                    tok_t((int)adobe_tokens::mul_op, 0, "*"),
                    tok_t(
                        (int)adobe_tokens::identifier,
                        0,
                        "original_doc_height"),
                    tok_t((int)adobe_tokens::mul_op, 0, "/"),
                    tok_t((int)adobe_tokens::number, 0, (long double)100.0),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "doc_height_percent"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "doc_height_inches"),
                    tok_t((int)adobe_tokens::mul_op, 0, "*"),
                    tok_t((int)adobe_tokens::number, 0, (long double)100.0),
                    tok_t((int)adobe_tokens::mul_op, 0, "/"),
                    tok_t(
                        (int)adobe_tokens::identifier,
                        0,
                        "original_doc_height"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)bp::character_id, 0, (long long)'}'),
                    tok_t((int)adobe_tokens::identifier, 0, "relate"),
                    tok_t((int)bp::character_id, 0, (long long)'{'),
                    tok_t((int)adobe_tokens::identifier, 0, "screen_lpi"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t((int)adobe_tokens::identifier, 0, "doc_resolution"),
                    tok_t((int)adobe_tokens::mul_op, 0, "/"),
                    tok_t((int)bp::character_id, 0, (long long)'('),
                    tok_t((int)adobe_tokens::identifier, 0, "auto_quality"),
                    tok_t((int)adobe_tokens::eq_op, 0, "=="),
                    tok_t((int)bp::character_id, 0, (long long)'@'),
                    tok_t((int)adobe_tokens::identifier, 0, "draft"),
                    tok_t((int)bp::character_id, 0, (long long)'?'),
                    tok_t((int)adobe_tokens::number, 0, (long double)1.0),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)bp::character_id, 0, (long long)'('),
                    tok_t((int)adobe_tokens::identifier, 0, "auto_quality"),
                    tok_t((int)adobe_tokens::eq_op, 0, "=="),
                    tok_t((int)bp::character_id, 0, (long long)'@'),
                    tok_t((int)adobe_tokens::identifier, 0, "good"),
                    tok_t((int)bp::character_id, 0, (long long)'?'),
                    tok_t((int)adobe_tokens::number, 0, (long double)1.5),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::number, 0, (long double)2.0),
                    tok_t((int)bp::character_id, 0, (long long)')'),
                    tok_t((int)bp::character_id, 0, (long long)')'),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, 0, "doc_resolution"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t((int)adobe_tokens::identifier, 0, "screen_lpi"),
                    tok_t((int)adobe_tokens::mul_op, 0, "*"),
                    tok_t((int)bp::character_id, 0, (long long)'('),
                    tok_t((int)adobe_tokens::identifier, 0, "auto_quality"),
                    tok_t((int)adobe_tokens::eq_op, 0, "=="),
                    tok_t((int)bp::character_id, 0, (long long)'@'),
                    tok_t((int)adobe_tokens::identifier, 0, "draft"),
                    tok_t((int)bp::character_id, 0, (long long)'?'),
                    tok_t((int)adobe_tokens::number, 0, (long double)1.0),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)bp::character_id, 0, (long long)'('),
                    tok_t((int)adobe_tokens::identifier, 0, "auto_quality"),
                    tok_t((int)adobe_tokens::eq_op, 0, "=="),
                    tok_t((int)bp::character_id, 0, (long long)'@'),
                    tok_t((int)adobe_tokens::identifier, 0, "good"),
                    tok_t((int)bp::character_id, 0, (long long)'?'),
                    tok_t((int)adobe_tokens::number, 0, (long double)1.5),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::number, 0, (long double)2.0),
                    tok_t((int)bp::character_id, 0, (long long)')'),
                    tok_t((int)bp::character_id, 0, (long long)')'),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)bp::character_id, 0, (long long)'}'),
                    tok_t((int)adobe_tokens::identifier, 0, "when"),
                    tok_t((int)bp::character_id, 0, (long long)'('),
                    tok_t((int)adobe_tokens::identifier, 0, "resample"),
                    tok_t((int)bp::character_id, 0, (long long)')'),
                    tok_t((int)adobe_tokens::identifier, 0, "relate"),
                    tok_t((int)bp::character_id, 0, (long long)'{'),
                    tok_t((int)adobe_tokens::identifier, 0, "dim_width_pixels"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "dim_width_percent"),
                    tok_t((int)adobe_tokens::mul_op, 0, "*"),
                    tok_t((int)adobe_tokens::identifier, 0, "original_width"),
                    tok_t((int)adobe_tokens::mul_op, 0, "/"),
                    tok_t((int)adobe_tokens::number, 0, (long double)100.0),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "dim_width_percent"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t((int)adobe_tokens::identifier, 0, "dim_width_pixels"),
                    tok_t((int)adobe_tokens::mul_op, 0, "*"),
                    tok_t((int)adobe_tokens::number, 0, (long double)100.0),
                    tok_t((int)adobe_tokens::mul_op, 0, "/"),
                    tok_t((int)adobe_tokens::identifier, 0, "original_width"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)bp::character_id, 0, (long long)'}'),
                    tok_t((int)adobe_tokens::identifier, 0, "when"),
                    tok_t((int)bp::character_id, 0, (long long)'('),
                    tok_t((int)adobe_tokens::identifier, 0, "resample"),
                    tok_t((int)bp::character_id, 0, (long long)')'),
                    tok_t((int)adobe_tokens::identifier, 0, "relate"),
                    tok_t((int)bp::character_id, 0, (long long)'{'),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "dim_height_pixels"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "dim_height_percent"),
                    tok_t((int)adobe_tokens::mul_op, 0, "*"),
                    tok_t((int)adobe_tokens::identifier, 0, "original_height"),
                    tok_t((int)adobe_tokens::mul_op, 0, "/"),
                    tok_t((int)adobe_tokens::number, 0, (long double)100.0),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "dim_height_percent"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "dim_height_pixels"),
                    tok_t((int)adobe_tokens::mul_op, 0, "*"),
                    tok_t((int)adobe_tokens::number, 0, (long double)100.0),
                    tok_t((int)adobe_tokens::mul_op, 0, "/"),
                    tok_t((int)adobe_tokens::identifier, 0, "original_height"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)bp::character_id, 0, (long long)'}'),
                    tok_t((int)adobe_tokens::identifier, 0, "when"),
                    tok_t((int)bp::character_id, 0, (long long)'('),
                    tok_t((int)adobe_tokens::identifier, 0, "resample"),
                    tok_t((int)bp::character_id, 0, (long long)')'),
                    tok_t((int)adobe_tokens::identifier, 0, "relate"),
                    tok_t((int)bp::character_id, 0, (long long)'{'),
                    tok_t((int)adobe_tokens::identifier, 0, "doc_width_inches"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t((int)adobe_tokens::identifier, 0, "dim_width_pixels"),
                    tok_t((int)adobe_tokens::mul_op, 0, "/"),
                    tok_t((int)adobe_tokens::identifier, 0, "doc_resolution"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, 0, "dim_width_pixels"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t((int)adobe_tokens::identifier, 0, "doc_width_inches"),
                    tok_t((int)adobe_tokens::mul_op, 0, "*"),
                    tok_t((int)adobe_tokens::identifier, 0, "doc_resolution"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, 0, "doc_resolution"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t((int)adobe_tokens::identifier, 0, "dim_width_pixels"),
                    tok_t((int)adobe_tokens::mul_op, 0, "/"),
                    tok_t((int)adobe_tokens::identifier, 0, "doc_width_inches"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)bp::character_id, 0, (long long)'}'),
                    tok_t((int)adobe_tokens::identifier, 0, "when"),
                    tok_t((int)bp::character_id, 0, (long long)'('),
                    tok_t((int)adobe_tokens::identifier, 0, "resample"),
                    tok_t((int)bp::character_id, 0, (long long)')'),
                    tok_t((int)adobe_tokens::identifier, 0, "relate"),
                    tok_t((int)bp::character_id, 0, (long long)'{'),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "doc_height_inches"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "dim_height_pixels"),
                    tok_t((int)adobe_tokens::mul_op, 0, "/"),
                    tok_t((int)adobe_tokens::identifier, 0, "doc_resolution"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "dim_height_pixels"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "doc_height_inches"),
                    tok_t((int)adobe_tokens::mul_op, 0, "*"),
                    tok_t((int)adobe_tokens::identifier, 0, "doc_resolution"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, 0, "doc_resolution"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "dim_height_pixels"),
                    tok_t((int)adobe_tokens::mul_op, 0, "/"),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "doc_height_inches"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)bp::character_id, 0, (long long)'}'),
                    tok_t((int)adobe_tokens::identifier, 0, "when"),
                    tok_t((int)bp::character_id, 0, (long long)'('),
                    tok_t((int)bp::character_id, 0, (long long)'!'),
                    tok_t((int)adobe_tokens::identifier, 0, "resample"),
                    tok_t((int)bp::character_id, 0, (long long)')'),
                    tok_t((int)adobe_tokens::identifier, 0, "relate"),
                    tok_t((int)bp::character_id, 0, (long long)'{'),
                    tok_t((int)adobe_tokens::identifier, 0, "doc_resolution"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t((int)adobe_tokens::identifier, 0, "original_width"),
                    tok_t((int)adobe_tokens::mul_op, 0, "/"),
                    tok_t((int)adobe_tokens::identifier, 0, "doc_width_inches"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, 0, "doc_width_inches"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t((int)adobe_tokens::identifier, 0, "original_width"),
                    tok_t((int)adobe_tokens::mul_op, 0, "/"),
                    tok_t((int)adobe_tokens::identifier, 0, "doc_resolution"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)bp::character_id, 0, (long long)'}'),
                    tok_t((int)adobe_tokens::identifier, 0, "when"),
                    tok_t((int)bp::character_id, 0, (long long)'('),
                    tok_t((int)bp::character_id, 0, (long long)'!'),
                    tok_t((int)adobe_tokens::identifier, 0, "resample"),
                    tok_t((int)bp::character_id, 0, (long long)')'),
                    tok_t((int)adobe_tokens::identifier, 0, "relate"),
                    tok_t((int)bp::character_id, 0, (long long)'{'),
                    tok_t((int)adobe_tokens::identifier, 0, "doc_resolution"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t((int)adobe_tokens::identifier, 0, "original_height"),
                    tok_t((int)adobe_tokens::mul_op, 0, "/"),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "doc_height_inches"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "doc_height_inches"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t((int)adobe_tokens::identifier, 0, "original_height"),
                    tok_t((int)adobe_tokens::mul_op, 0, "/"),
                    tok_t((int)adobe_tokens::identifier, 0, "doc_resolution"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)bp::character_id, 0, (long long)'}'),
                    tok_t((int)adobe_tokens::identifier, 0, "when"),
                    tok_t((int)bp::character_id, 0, (long long)'('),
                    tok_t((int)adobe_tokens::identifier, 0, "constrain"),
                    tok_t((int)adobe_tokens::and_, 0, "&&"),
                    tok_t((int)adobe_tokens::identifier, 0, "resample"),
                    tok_t((int)bp::character_id, 0, (long long)')'),
                    tok_t((int)adobe_tokens::identifier, 0, "relate"),
                    tok_t((int)bp::character_id, 0, (long long)'{'),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "dim_width_percent"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "dim_height_percent"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "dim_height_percent"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "dim_width_percent"),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)bp::character_id, 0, (long long)'}'),
                    tok_t((int)adobe_tokens::identifier, 0, "output"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, 0, "byte_count"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t((int)adobe_tokens::identifier, 0, "dim_width_pixels"),
                    tok_t((int)adobe_tokens::mul_op, 0, "*"),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "dim_height_pixels"),
                    tok_t((int)adobe_tokens::mul_op, 0, "*"),
                    tok_t((int)adobe_tokens::number, 0, (long double)32.0),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, 0, "result"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t((int)adobe_tokens::identifier, 0, "resample"),
                    tok_t((int)bp::character_id, 0, (long long)'?'),
                    tok_t((int)bp::character_id, 0, (long long)'{'),
                    tok_t((int)adobe_tokens::identifier, 0, "command"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)bp::character_id, 0, (long long)'@'),
                    tok_t((int)adobe_tokens::identifier, 0, "resize_image"),
                    tok_t((int)bp::character_id, 0, (long long)','),
                    tok_t((int)adobe_tokens::identifier, 0, "width"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, 0, "dim_width_pixels"),
                    tok_t((int)bp::character_id, 0, (long long)','),
                    tok_t((int)adobe_tokens::identifier, 0, "height"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "dim_height_pixels"),
                    tok_t((int)bp::character_id, 0, (long long)','),
                    tok_t((int)adobe_tokens::identifier, 0, "resolution"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, 0, "doc_resolution"),
                    tok_t((int)bp::character_id, 0, (long long)','),
                    tok_t((int)adobe_tokens::identifier, 0, "scale_styles"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, 0, "scale_styles"),
                    tok_t((int)bp::character_id, 0, (long long)','),
                    tok_t((int)adobe_tokens::identifier, 0, "resample_method"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, 0, "resample_method"),
                    tok_t((int)bp::character_id, 0, (long long)'}'),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)bp::character_id, 0, (long long)'{'),
                    tok_t((int)adobe_tokens::identifier, 0, "command"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)bp::character_id, 0, (long long)'@'),
                    tok_t((int)adobe_tokens::identifier, 0, "set_resolution"),
                    tok_t((int)bp::character_id, 0, (long long)','),
                    tok_t((int)adobe_tokens::identifier, 0, "resolution"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, 0, "doc_resolution"),
                    tok_t((int)bp::character_id, 0, (long long)'}'),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, 0, "invariant"),
                    tok_t((int)bp::character_id, 0, (long long)':'),
                    tok_t((int)adobe_tokens::identifier, 0, "width_max"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t((int)adobe_tokens::identifier, 0, "dim_width_pixels"),
                    tok_t((int)adobe_tokens::rel_op, 0, "<"),
                    tok_t((int)bp::character_id, 0, (long long)'='),
                    tok_t((int)adobe_tokens::number, 0, (long double)300000.0),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)adobe_tokens::identifier, 0, "height_max"),
                    tok_t((int)adobe_tokens::define, 0, "<=="),
                    tok_t(
                        (int)adobe_tokens::identifier, 0, "dim_height_pixels"),
                    tok_t((int)adobe_tokens::rel_op, 0, "<"),
                    tok_t((int)bp::character_id, 0, (long long)'='),
                    tok_t((int)adobe_tokens::number, 0, (long double)300000.0),
                    tok_t((int)bp::character_id, 0, (long long)';'),
                    tok_t((int)bp::character_id, 0, (long long)'}')};

                int position = 0;
                for (auto tok :
                     std::string(large_input) | bp::to_tokens(adobe_lexer)) {
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
