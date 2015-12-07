/**
 * @file  exs64m_nif.c
 * @brief XorShift64star NIF experimental library
 *
 * @author Kenji Rikitake
 * @author Sebastiano Vigna
 */

/**
 * Sebastiano Vigna applies CC0 (public domain) license
 * for his Xorshift64star C example code.
 */

/** 
 * (MIT License)
 *
 * Copyright (c) 2014-2015 Kenji Rikitake. All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>

#include "erl_nif.h"

/** Version number for load_info. */
#define NIF_LOAD_INFO (101)

/* prototypes */
static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

static ERL_NIF_TERM exs64m_nif_get_lib_refc(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM exs64m_nif_nif_next_list(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

/* internal structure */

struct EXS64_RETVAL {
    uint64_t x;
    uint64_t state;
};
typedef struct EXS64_RETVAL exs64_retval;

static inline exs64_retval next(uint64_t state);

/** Number of users of this dynamic library. */
static int lib_refc = 0;

/** Function list passed to the Erlang BEAM for this NIF. */
static ErlNifFunc nif_funcs[] = {
    {"get_lib_refc", 0, exs64m_nif_get_lib_refc},
    {"nif_next_list", 2, exs64m_nif_nif_next_list}
};

/* Function call macro to initialize NIF. */
ERL_NIF_INIT(exs64m, nif_funcs, load, NULL, upgrade, unload)

/**
 * Checks the version number of the load info from Erlang.
 * See is_ok_load_info() in c_src/crypto.c of the crypto module.
 * @param env ErlNifEnv pointer for the calling process.
 * @param load_info ERL_NIF_TERM to identify the NIF library.
 */
static int check_load_info(ErlNifEnv* env, ERL_NIF_TERM load_info)
{
    int i;

    /* check the version number of the load info */
    return (enif_get_int(env,load_info,&i) && i == NIF_LOAD_INFO);
}

/**
 * Loads NIF module and defines Erlang atoms.
 * @param env ErlNifEnv pointer for the calling process.
 * @param priv_data pointing the private data for the NIF library to keep between the NIF calls.
 * @param load_info ERL_NIF_TERM to identify the NIF library.
 */
static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    /* checking version number on the argument load_info */
    if (!check_load_info(env, load_info)) {
        return -1;
    }
    *priv_data = NULL;
    /* increase the reference count of this library */
    lib_refc++;

    return 0;
}

/**
 * Upgrades NIF module.
 * @param env ErlNifEnv pointer for the calling process.
 * @param priv_data pointing the private data for the NIF library to keep between the NIF calls.
 * @param old_priv_data pointing the private data given from the last calls of load() or reload().
 * @param load_info ERL_NIF_TERM to identify the NIF library.
 */
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    /* Don't know how to do this */
    if (*old_priv_data != NULL) {
            return -1;
    }
    /* Don't know how to do this */
    if (*priv_data != NULL) {
            return -1;
    }

    /* checking version number on the argument load_info */
    if (!check_load_info(env, load_info)) {
        return -1;
    }
    /* increase the reference count of this library */
    lib_refc++;

    return 0;
}

/**
 * Unloads NIF module.
 * @param env ErlNifEnv pointer for the calling process.
 * @param priv_data pointing the private data for the NIF library to keep between the NIF calls.
 */
static void unload(ErlNifEnv* env, void* priv_data)
{
    /* this is yet a skeleton code */
    /* perform resource unlock and deallocation here,
       but in this module no resource are retained
       outside of the scope of each function */
    if (--lib_refc <= 0) {
        /* do nothing */
    }
    /*else NIF library still used by other (new) module code */
}

/* NIF code follows */

/**
 * NIF code of get_lib_refc/0.
 * @param env ErlNifEnv pointer for the calling process.
 * @param argc Erlang function arity.
 * @param argv ERL_NIF_TERM pointers for the arguments.
 */
static ERL_NIF_TERM
exs64m_nif_get_lib_refc(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{ /* () */
    return enif_make_int(env, lib_refc);
}

static ERL_NIF_TERM
exs64m_nif_nif_next_list(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{ /* uint64(), state() */
    ErlNifUInt64 len, state;
    uint64_t i;
    ERL_NIF_TERM *terms, list;
    exs64_retval new;
    
    if (!enif_get_uint64(env, argv[0], &len)
        || len == 0LL
        || !enif_get_uint64(env, argv[1], &state)
        || state == 0LL ) {
        return enif_make_badarg(env);
    }

    terms = (ERL_NIF_TERM *) enif_alloc(len * sizeof(ERL_NIF_TERM *));
    if (NULL == terms) {
        return enif_make_badarg(env);
    }

    for (i = 0LL; i < (uint64_t)len; i++) {
        new = next((uint64_t)state);
        terms[i] = enif_make_uint64(env, new.x);
        state = (ErlNifUInt64)new.state;
    }

    list = enif_make_list_from_array(env, terms, len);

    enif_free(terms);

    return enif_make_tuple2(
             env,
             list,
             enif_make_uint64(env, state));
}

/* exs64m static C function code follows */
/* The state must be seeded with a nonzero value. */

static inline exs64_retval next(uint64_t s) {
    exs64_retval new;
    
	s ^= s >> 12;
	s ^= s << 25;
	s ^= s >> 27;

	new.x = s * 2685821657736338717LL;
    new.state = s;

    return new;
}
