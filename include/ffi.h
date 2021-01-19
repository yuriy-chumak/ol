/*
 * ffi.h
 *
 *  Created on: Sep 8, 2015
 *  Copyright (c) 2015-2021
 *      Author: uri
 */
#pragma once

struct heap_t;
#define ol_t heap_t

#define USE_OLVM_DECLARATION
#include "olvm.c"

#define unless(...) if (! (__VA_ARGS__))

typedef struct ol_t OL;
