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
#include <map>

#include "cxxtest.hpp"

using namespace std;

static void cx11_lambda_test(void)
{
    cout << "cx11 lambda" << endl;

    std::vector<int> v;
    v.push_back(1);
    v.push_back(2);
    v.push_back(3);

    std::for_each(std::begin(v), std::end(v), [](int n) {std::cout << n << std::endl;});

    auto is_odd = [](int n) {return n%2==1;};
    auto pos = std::find_if(std::begin(v), std::end(v), is_odd);
    if(pos != std::end(v))
      std::cout << *pos << std::endl;
}

static void cx11_ranged_forloop_test(void)
{
    cout << "cx11 ranged forloop" << endl;

    std::map<std::string, std::vector<int>> map;
    std::vector<int> v, v2;
    v.push_back(1);
    v.push_back(2);
    v.push_back(3);
    v2.push_back(4);
    v2.push_back(5);
    v2.push_back(6);
    v2.push_back(7);
    map["one"] = v;
    map["two"] = v2;

    for(const auto& kvp : map)
    {
      std::cout << kvp.first << std::endl;

      for(auto val : kvp.second)
      {
         std::cout << " " << val << std::endl;
      }
    }

    int arr[] = {1,2,3,4,5};
    for(int& e : arr)
    {
      e = e*e;
    }
}

void cx11_test(void)
{
    cout << "cx11_test" << endl;

    cx11_ranged_forloop_test();
    cx11_lambda_test();
}
