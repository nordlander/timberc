// The Timber compiler <timber-lang.org>
// 
// Copyright 2008 Johan Nordlander <nordland@csee.ltu.se>
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
// 
// 3. Neither the names of the copyright holder and any identified
//    contributors, nor the names of their affiliations, may be used to 
//    endorse or promote products derived from this software without 
//    specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
// OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
// OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
// STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
// ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

#ifndef ENV_H_
#define ENV_H_

#define SOCKHANDLER   CLOS2_Prelude   // CLOS_Peer_POSIX_Int_Connection_POSIX__POSIX
#define HANDLER       CLOS3_POSIX     // CLOS_LIST_Time_Time_Msg__POSIX
#define ACTION        CLOS2_Prelude   // CLOS_Time_Time_Msg__POSIX
#define FILE2CLOSABLE l_File_POSIX_Closable_POSIX_POSIX
#define RFILE2FILE    l_RFile_POSIX_File_POSIX_POSIX
#define WFILE2FILE    l_WFile_POSIX_File_POSIX_POSIX
#define CONN2CLOSABLE l_Connection_POSIX_Closable_POSIX_POSIX
#define SOCK2CLOSABLE l_Socket_POSIX_Closable_POSIX_POSIX
#define __GC__HANDLER __GC__CLOS3_POSIX    // __GC__CLOS_LIST_Time_Time_Msg__POSIX
#endif
