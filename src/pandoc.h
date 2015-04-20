/*
 * Copyright (C) 2009-2010  Anton Tayanovskyy <name.surname@gmail.com>
 * Copyright (C) 2015  Shahbaz Youssefi <ShabbyX@gmail.com>
 *
 * This file is part of libpandoc, providing C bindings to Pandoc.
 *
 * libpandoc is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * libpandoc is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with libpandoc.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _PANDOC_H
#define _PANDOC_H 1

#include <wchar.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef int (*reader_t)(char *, int, void *);
typedef void (*writer_t)(const char *, int, void *);

#ifndef WIN32
/*
 * Initializes the Haskell runtime. Every call to this function should
 * be matched with exactly one call to `pandoc_exit`.
 */
int pandoc_init();

  
/* Shuts down the Haskell runtime.  */
void pandoc_exit();
#endif
  
/*
 * Calls `pandoc` with given input and output formats and streams.
 * Returns a `NULL` on success, or a `NULL`-terminated error message
 * on failure.  Settings is an XML string conforming to a schema
 * distributed with `libpandoc`.  Settings can be `NULL`.  All strings
 * should be encoded as UTF-8.  User data is any pointer.
 */
char *pandoc(int buffer_size,
            const char *input_format, const char *output_format, const char *settings,
            reader_t reader, writer_t writer, void *user_data);

int valid_language(const char* language);
  
char *highlight(int buffer_size, const char *language, const char *output_format,
                int block, reader_t reader, writer_t writer, void *user_data);

#ifdef __cplusplus
}
#endif

#endif /* !_PANDOC_H */
