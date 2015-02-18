/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <iostream>
#include <vector>
#include <string>

#include "cxxtest.hpp"

using namespace std;

static void stl_vector_iter(void)
{
    std::cout << "STL Vector: stl_vector_iter" << std::endl;

    vector<vector<int> > vI2Matrix;    // Declare two dimensional array
    vector<int> A, B;
    vector<vector<int> >::iterator iter_ii;
    vector<int>::iterator iter_jj;

    A.push_back(10);
    A.push_back(20);
    A.push_back(30);
    B.push_back(100);
    B.push_back(200);
    B.push_back(300);

    vI2Matrix.push_back(A);
    vI2Matrix.push_back(B);

    cout << endl << "Using Iterator:" << endl;

    for (iter_ii = vI2Matrix.begin(); iter_ii != vI2Matrix.end(); iter_ii++) {
        for (iter_jj = (*iter_ii).begin(); iter_jj != (*iter_ii).end();
                        iter_jj++) {
            cout << *iter_jj << endl;
        }
    }
}

static void stl_vector_2d(void)
{
    std::cout << "STL Vector: stl_vector_2d" << std::endl;

    // Declare size of two dimensional array and initialize.
    vector<vector<int> > vI2Matrix(3, vector<int>(2, 0));

    vI2Matrix[0][0] = 0;
    vI2Matrix[0][1] = 1;
    vI2Matrix[1][0] = 10;
    vI2Matrix[1][1] = 11;
    vI2Matrix[2][0] = 20;
    vI2Matrix[2][1] = 21;

    cout << "Loop by index:" << endl;

    int ii, jj;
    for (ii = 0; ii < 3; ii++) {
        for (jj = 0; jj < 2; jj++) {
            cout << vI2Matrix[ii][jj] << endl;
        }
    }
}

static void check(string val,
                  uint32_t index)
{
    switch (index) {
        case 0:
            if (val.compare("apple") != 0) {
                cout << "invalid entry: " << val << " / apple" << endl;
                abort();
            }
            break;
        case 1:
            if (val.compare("banana") != 0) {
                cout << "invalid entry: " << val << " / banana" << endl;
                abort();
            }
            break;
        case 2:
            if (val.compare("apricot") != 0) {
                cout << "invalid entry: " << val << " / apricot" << endl;
                abort();
            }
            break;
        default:
            cout << "invalid fruit vector size!" << endl;
            abort();
            break;
    }
}

static void stl_vector_test_strings(void)
{
    std::cout << "STL Vector: stl_vector_test_strings" << std::endl;

    vector<string> fruits;

    fruits.push_back("apple");
    fruits.push_back("banana");
    fruits.push_back("apricot");

    cout << "  ~ loop index: ";

    for (uint32_t ii = 0; ii < fruits.size(); ii++) {
        check(fruits[ii], ii);
    }

    cout << "OK." << endl << "  ~ constant iterator: ";

    vector<string>::const_iterator cii;
    uint32_t i = 0;
    for (cii = fruits.begin(); cii != fruits.end(); cii++) {
        check(*cii, i++);
    }

    cout << "OK." << endl << "  ~ reverse iterator: ";

    vector<string>::reverse_iterator rii;
    i = fruits.size();
    for (rii = fruits.rbegin(); rii != fruits.rend(); ++rii) {
        check(*rii, --i);
    }

    cout << "OK." << endl;
}

#define STL_VEC_SIMPLE_MAX 100

static void stl_vector_test_simple(void)
{
    std::cout << "STL Vector: stl_vector_test_simple" << std::endl;

    vector<uint32_t> v;

    for (uint32_t i = 0; i < STL_VEC_SIMPLE_MAX; ++i) {
        v.push_back(i);
    }

    cout << "  ~ looped index: ";

    for (uint32_t ii = 0; ii < v.size(); ii++) {
        if (v[ii] != ii) {
            cout << "Value mismatch: " << v[ii] << " / " << ii << endl;
            abort();
        }
    }

    cout << "OK." << endl << "  ~ const iterator: ";

    vector<uint32_t>::const_iterator cii;
    uint32_t i = 0;
    for (cii = v.begin(); cii != v.end(); cii++) {
        if (*cii != i) {
            cout << "Value mismatch: " << *cii << " / " << i << endl;
            abort();
        }
        i++;
    }

    cout << "OK." << endl << "  ~ swapping values" << endl;

    for (uint32_t ii = 0, jj = STL_VEC_SIMPLE_MAX - 1; ii < v.size() / 2;
                    ii++, jj--) {
        swap(v[ii], v[jj]);
    }

    cout << "  ~ reverse iterator: ";

    vector<uint32_t>::reverse_iterator rii;
    i = 0;
    for (rii = v.rbegin(); rii != v.rend(); ++rii) {
        if (*rii != i) {
            cout << "Value mismatch: " << *rii << " / " << i << endl;
            abort();
        }
        i++;
    }
    cout << "OK." << endl;
}

static void stl_vector_test_emplace(void)
{
    std::cout << "STL vector: emplace test: ";

    vector<int> v;
    v.emplace_back(0);
    v.emplace_back(1);
    int i = 0;
    for (auto it = v.begin(); it != v.end(); ++it, ++i) {
        if (*it != i) {
            cout << "Value mismatch: " << *it << " / " << i << endl;
            abort();
        }
    }
    cout << "OK." << endl;
    return;
}

void stl_vector_test(void)
{
    std::cout << "STL Vector tests" << std::endl;
    stl_vector_test_simple();
    stl_vector_test_strings();
    stl_vector_2d();
    stl_vector_iter();
    stl_vector_test_emplace();
}
