/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <iostream>
#include <list>
#include <string>

#include "cxxtest.hpp"

using namespace std;

class AAA
{
    friend ostream &operator<<(ostream &,
                               const AAA &);

public:
    int x;
    int y;
    float z;

    AAA();
    AAA(const AAA &);
    ~AAA()
    {
    }
    ;
    AAA &operator=(const AAA &rhs);
    int operator==(const AAA &rhs) const;
    int operator<(const AAA &rhs) const;
};

AAA::AAA()   // Constructor
{
    x = 0;
    y = 0;
    z = 0;
}

AAA::AAA(const AAA &copyin)   // Copy constructor to handle pass by value.
{
    x = copyin.x;
    y = copyin.y;
    z = copyin.z;
}

ostream &operator<<(ostream &output,
                    const AAA &aaa)
{
    output << aaa.x << ' ' << aaa.y << ' ' << aaa.z << endl;
    return output;
}

AAA& AAA::operator=(const AAA &rhs)
{
    this->x = rhs.x;
    this->y = rhs.y;
    this->z = rhs.z;
    return *this;
}

int AAA::operator==(const AAA &rhs) const
{
    if (this->x != rhs.x)
        return 0;
    if (this->y != rhs.y)
        return 0;
    if (this->z != rhs.z)
        return 0;
    return 1;
}

// This function is required for built-in STL list functions like sort
int AAA::operator<(const AAA &rhs) const
{
    if (this->x == rhs.x && this->y == rhs.y && this->z < rhs.z)
        return 1;
    if (this->x == rhs.x && this->y < rhs.y)
        return 1;
    if (this->x < rhs.x)
        return 1;
    return 0;
}

static void stl_list_test_double(void)
{
    std::cout << "STL List: stl_list_test_double" << std::endl;

    list<AAA> L;
    AAA Ablob;

    Ablob.x = 7;
    Ablob.y = 2;
    Ablob.z = 4.2355;
    L.push_back(Ablob);  // Insert a new element at the end

    Ablob.x = 5;
    L.push_back(Ablob);  // Object passed by value. Uses default member-wise
                         // copy constructor
    Ablob.z = 3.2355;
    L.push_back(Ablob);

    Ablob.x = 3;
    Ablob.y = 7;
    Ablob.z = 7.2355;
    L.push_back(Ablob);

    list<AAA>::iterator i;

    for (i = L.begin(); i != L.end(); ++i)
        cout << (*i).x << " ";  // print member
    cout << endl;

    for (i = L.begin(); i != L.end(); ++i)
        cout << *i << " ";  // print with overloaded operator
    cout << endl;

    cout << "Sorted: " << endl;
    L.sort();
    for (i = L.begin(); i != L.end(); ++i)
        cout << *i << " ";  // print with overloaded operator
    cout << endl;
}

static void stl_list_test_simple(void)
{
    std::cout << "STL List: stl_list_test_simple" << std::endl;
    list<int> L;
    L.push_back(0);              // Insert a new element at the end
    L.push_front(0);             // Insert a new element at the beginning
    L.insert(++L.begin(), 2);     // Insert "2" before position of first argument
                                  // (Place before second argument)
    L.push_back(5);
    L.push_back(6);

    list<int>::iterator i;

    for (i = L.begin(); i != L.end(); ++i)
        cout << *i << " ";
    cout << endl;
}

void stl_list_test(void)
{
    std::cout << "STL List tests" << std::endl;
    stl_list_test_simple();
    stl_list_test_double();
}
