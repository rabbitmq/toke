/* ------------------------------------------------------------------------- */
/*                                                                           */
/*   The contents of this file are subject to the Mozilla Public License     */
/*   Version 1.1 (the "License"); you may not use this file except in        */
/*   compliance with the License. You may obtain a copy of the License at    */
/*   http://www.mozilla.org/MPL/                                             */
/*                                                                           */
/*   Software distributed under the License is distributed on an "AS IS"     */
/*   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the */
/*   License for the specific language governing rights and limitations      */
/*   under the License.                                                      */
/*                                                                           */
/*   The Original Code is Toke.                                              */
/*                                                                           */
/*   The Initial Developers of the Original Code are GoPivotal, Inc.            */
/*   Copyright (c) 2009-2014 GoPivotal, Inc.  All rights reserved.              */
/*                                                                           */
/* ------------------------------------------------------------------------- */

#ifndef __TOKE_H_
#define __TOKE_H_

enum _CommandType {
  TOKE_INVALID_COMMAND = 255,
  TOKE_NEW             = 0,
  TOKE_DEL             = 1,
  TOKE_TUNE            = 2,
  TOKE_SET_CACHE       = 3,
  TOKE_SET_XM_SIZE     = 4,
  TOKE_SET_DF_UNIT     = 5,
  TOKE_OPEN            = 6,
  TOKE_CLOSE           = 7,
  TOKE_INSERT          = 8,
  TOKE_INSERT_NEW      = 9,
  TOKE_INSERT_CONCAT   = 10,
  TOKE_INSERT_ASYNC    = 11,
  TOKE_DELETE          = 12,
  TOKE_DELETE_IF_EQ    = 13,
  TOKE_GET             = 14,
  TOKE_GET_ALL         = 15
};
typedef enum _CommandType CommandType;

enum _ReaderError {
  READER_NO_ERROR = 0,
  READER_READ_ALL_DATA = 1,
  READER_PACKING_ERROR = 2
};
typedef enum _ReaderError ReaderError;

/* These are duplicated here from tchdb.h so that I only need to keep
   one file in sync with the Erlang, and not the TokyoCabinet headers too */
enum _OpenMode {              /* enumeration for open modes */
  TOKE_OPEN_READER = 1 << 0,  /* open as a reader */
  TOKE_OPEN_WRITER = 1 << 1,  /* open as a writer */
  TOKE_OPEN_CREAT  = 1 << 2,  /* writer creating */
  TOKE_OPEN_TRUNC  = 1 << 3,  /* writer truncating */
  TOKE_OPEN_NOLCK  = 1 << 4,  /* open without locking */
  TOKE_OPEN_LCKNB  = 1 << 5,  /* lock without blocking */
  TOKE_OPEN_TSYNC  = 1 << 6   /* synchronize every transaction */
};

enum _TuneOpts {              /* enumeration for tuning options */
  TOKE_TUNE_LARGE   = 1 << 0, /* use 64-bit bucket array */
  TOKE_TUNE_DEFLATE = 1 << 1, /* compress each record with Deflate */
  TOKE_TUNE_BZIP    = 1 << 2, /* compress each record with BZIP2 */
  TOKE_TUNE_TCBS    = 1 << 3, /* compress each record with TCBS */
  TOKE_TUNE_EXCODEC = 1 << 4  /* compress each record with custom functions */
};


#endif
