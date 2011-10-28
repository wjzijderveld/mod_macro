/*
 * The following license applies to "mod_macro" version 1.1.11.
 * It is a third-party module by Fabien Coelho <mod.macro@coelho.net>
 * for the Apache Http Server (http://www.apache.org/).
 *
 * This license is basically the same as apache, with the small
 * additionnal provision that I like to receive postcards;-)
 *
 * ====================================================================
 * Copyright (c) 1998-2010 Fabien Coelho. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution,
 *    if any, must include the following acknowledgment:
 *    "This product includes software developed by
 *    Fabien Coelho <mod.macro@coelho.net>
 *    for use in the mod_macro project
 *    (http://www.coelho.net/mod_macro/)."
 *    Alternately, this acknowledgment may appear in the software itself,
 *    if and wherever such third-party acknowledgments normally appear.
 *
 * 4. The name "mod_macro" must not be used to
 *    endorse or promote products derived from this software without
 *    prior written permission. For written permission, please contact
 *    Fabien Coelho <mod.macro@coelho.net>.
 *
 * 5. Products derived from this software may not be called "mod_macro"
 *    nor may "mod_macro" appear in their names without prior written
 *    permission of Fabien Coelho.
 *
 * 6. If you are very happy with this module, I would appreciate if you
 *    could send me a postcard (see my web page for current address).
 *
 * THIS SOFTWARE IS PROVIDED BY FABIEN COELHO ``AS IS'' AND ANY
 * EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE APACHE GROUP OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 * ====================================================================
 */
/*
  $Id: mod_macro.src.c 577 2008-11-04 14:43:45Z coelho $

  mod_macro version 1.1.11.

  This modules allows the definition and use of macros within apache
  runtime configuration files. Patch suggestions may be sent to the
  author.

  Fabien Coelho <mod.macro@coelho.net>.
  URL: http://www.coelho.net/

  TODO: use new configuration stuff...
   - ap_pcfg_custom_config()
   - array stuff processing for -c/-C
*/

#include "httpd.h"
#include "http_config.h"
#include "http_log.h"

#include "apr.h"
#include "apr_strings.h"
#include "apr_portable.h"
#include "apr_file_io.h"

typedef enum { mm_false, mm_true } mm_boolean;

/************************************************ COMPILE TIME DEBUG CONTROL */
/*
   debug:
   #define MOD_MACRO_DEBUG 1

   gdb:
   run -f /users/cri/coelho/Misc/apache/mod_macro/Tests/test??.conf

   no warnings:
   #define MOD_MACRO_NO_WARNINGS 1
   #define MOD_MACRO_NO_CHAR_PREFIX_WARNINGS 1

   no advertisement in version component:
   #define MOD_MACRO_NO_ADVERTISEMENT 1
*/
#undef MOD_MACRO_DEBUG

#if defined(debug)
#undef debug
#endif

#if defined(MOD_MACRO_DEBUG)
#define debug(stmt) stmt
#else
#define debug(stmt)
#endif

/************************************************************* ADVERTISEMENT */

module AP_MODULE_DECLARE_DATA macro_module;

#define MACRO_MODULE_NAME "mod_macro"
#define MACRO_MODULE_VERSION  "1.1.11"

/********************************************************** MACRO MANAGEMENT */

/* a macro: name, arguments, contents, location.
 */
typedef struct {
  char * name;        /* case-insensitive name of the macro. */
  apr_array_header_t * arguments; /* of char* */
  apr_array_header_t * contents;
  char * location;        /* of the macro definition. */
} macro_t;

/* configuration tokens.
 */
#define BEGIN_MACRO "<Macro"
#define END_MACRO "</Macro>"
#define USE_MACRO "Use"

#define empty_string_p(p) (!(p) || *(p) == '\0')

/* macros are kept globally...
   they are not per-server or per-directory entities.

   I would need a hook BEFORE and AFTER configuration processing
   to initialize and close them properly.

   I would have such a hook if in the main/... I guess.

   The "initializer" does not seem to be called before.

   HACK:
   -----
   I put them in the temp_pool.
   restarts are detected because the temp_pool has changed...
   note that there is always a restart implicitely to check for the syntax.
*/
static apr_array_header_t * all_macros = NULL;

/* returns the macro structure for name, or NULL if not found.
 */
static macro_t * get_macro_by_name(const apr_array_header_t * macros,
           const char * name)
{
  int i;
  macro_t ** tab;
  ap_assert(macros);
  tab = (macro_t **)macros->elts;

  for (i = 0; i < macros->nelts; i++) {
    if (!strcasecmp(name, tab[i]->name)) {
      return tab[i];
    }
  }
  return NULL;
}

/* configuration state initialization.
   the state is simply an apr_array_header_t which holds the macros.
 */
static void macro_init(apr_pool_t * p)
{
  static apr_pool_t * last_time_temp_pool_hack = NULL;

  debug(fprintf(stderr, "macro_init\n"));

  /* is it a restart? what about concurrent threads?
   */
  if (last_time_temp_pool_hack != p) {
    last_time_temp_pool_hack = p;
    all_macros = apr_array_make(p, 1, sizeof(macro_t *));
    debug(fprintf(stderr, "macro_init done for %p\n", p));
  }
}

/*************************************************************** PARSE UTILS */

#define trim(line) while (*(line)==' ' || *(line)=='\t') (line)++

/* return configuration-parsed arguments from line as an array.
   the line is expected not to contain any '\n'?
 */
static apr_array_header_t * get_arguments(
  apr_pool_t * p,
  const char * line)
{
  apr_array_header_t * args = apr_array_make(p, 1, sizeof(char *));
  char * arg, ** new;

  trim(line);
  while (*line) {
    arg = ap_getword_conf(p, &line);
    new = apr_array_push(args);
    *new = arg;
    trim(line);
  }

  return args;
}

/* get read lines as an array till end_token.
   counts nesting for begin_token/end_token.
   it assumes a line-per-line configuration (thru getline).
   this function could be exported.
   begin_token may be NULL.
*/
static char * get_lines_till_end_token(
  apr_pool_t * p,
  ap_configfile_t * config_file,
  const char * end_token,
  const char * begin_token,
  const char * where,
  apr_array_header_t ** plines)
{
  apr_array_header_t * lines = apr_array_make(p, 1, sizeof(char *));
  char ** new, * first, * ptr;
  char line[MAX_STRING_LEN]; /* sorry, but that is expected by getline. */
  int macro_nesting = 1, any_nesting = 1, line_number = 0;

  while (!ap_cfg_getline(line, MAX_STRING_LEN, config_file)) {
    ptr = line;
    /* first char? or first non blank? */
    if (*line=='#') continue;
    first = ap_getword_conf_nc(p, &ptr);
    line_number++;
    if (first) {
      /* nesting... */
      if (!strncmp(first, "</", 2)) {
        any_nesting--;
#if !defined(MOD_MACRO_NO_WARNINGS)
        if (any_nesting<0) {
          ap_log_error(APLOG_MARK, APLOG_NOERRNO|APLOG_WARNING,
                       0, NULL,
                       "bad (negative) nesting on line %d of %s",
                       line_number, where);
        }
#endif
      }
      else if (!strncmp(first, "<", 1)) {
        any_nesting++;
      }

      if (!strcasecmp(first, end_token)) { /* okay! */
        macro_nesting--;
        if (!macro_nesting) {
#if !defined(MOD_MACRO_NO_WARNINGS)
          if (any_nesting) {
            ap_log_error(APLOG_MARK, APLOG_NOERRNO|APLOG_WARNING,
                         0, NULL,
                         "bad cumulated nesting (%+d) in %s",
                         any_nesting, where);
          }
#endif
          *plines = lines;
          return NULL;
        }
      }
      else if (begin_token && !strcasecmp(first, begin_token)) {
        macro_nesting++;
      }
    }
    /* free first. */
    new  = apr_array_push(lines);
    *new = apr_psprintf(p, "%s\n", line); /* put '\n' back */
  }

  return apr_psprintf(p, "expected token not found: %s", end_token);
}

#define ESCAPE_ARG '@'
#define ARG_PREFIX "$%@"

/* characters allowed in an argument? not used yet.
 */
#define ARG_CONTENT              \
  "abcdefghijklmnopqrstuvwxyz"   \
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"   \
  "0123456789_" ARG_PREFIX

/* returns whether it looks like an argument, i.e. prefixed by ARG_PREFIX.
 */
static mm_boolean looks_like_an_argument(const char * word)
{
  return (mm_boolean) ap_strchr(ARG_PREFIX, *word);
}

/* generates an error on macro with two arguments of the same name.
   generates an error if a macro argument name is empty.
   generates a warning if arguments name prefixes conflict.
   generates a warning if the first char of an argument is not in ARG_PREFIX
*/
static const char * check_macro_arguments(
  apr_pool_t * p,
  const macro_t * macro)
{
  char ** tab = (char **)macro->arguments->elts;
  int nelts = macro->arguments->nelts, i, j;
  size_t ltabi, ltabj;

  for (i = 0; i < nelts; i++) {
    ltabi = strlen(tab[i]);

    if (ltabi == 0) {
      return apr_psprintf(p,
                          "macro '%s' (%s): empty argument #%d name",
                          macro->name, macro->location, i + 1);
    }

#if !defined(MOD_MACRO_NO_CHAR_PREFIX_WARNINGS) ||  \
  !defined(MOD_MACRO_NO_WARNINGS)
    if (!looks_like_an_argument(tab[i])) {
      ap_log_error(APLOG_MARK, APLOG_NOERRNO|APLOG_WARNING, 0, NULL,
                   "macro '%s' (%s) "
                   "argument name '%s' (#%d) without expected prefix, "
                   "better prefix argument names with one of '%s'.",
                   macro->name, macro->location,
                   tab[i], i + 1, ARG_PREFIX);
    }
#endif

    for (j = i + 1; j < nelts; j++) {
      ltabj = strlen(tab[j]);

      if (!strcmp(tab[i], tab[j])) {
        return apr_psprintf(p,
                            "argument name conflict in macro '%s' (%s): "
                            "argument '%s': #%d and #%d, "
                            "change argument names!",
                            macro->name, macro->location,
                            tab[i], i + 1, j + 1);
      }

#if !defined(MOD_MACRO_NO_WARNINGS)
      if (!strncmp(tab[i], tab[j], ltabi < ltabj ? ltabi : ltabj)) {
        ap_log_error(APLOG_MARK, APLOG_NOERRNO|APLOG_WARNING,
                     0, NULL,
                     "macro '%s' (%s): "
                     "argument name prefix conflict (%s #%d and %s #%d), "
                     "be careful about your macro definition!",
                     macro->name, macro->location,
                     tab[i], i + 1, tab[j], j + 1);
      }
#endif
    }
  }

  return NULL;
}

/* warn about empty strings in array.
 */
static void check_macro_use_arguments(
  const char * where,
  const apr_array_header_t * array)
{
  int i;
  char ** tab = (char **)array->elts;

#if !defined(MOD_MACRO_NO_WARNINGS)
  for (i = 0; i < array->nelts; i++) {
    if (empty_string_p(tab[i])) {
      ap_log_error(APLOG_MARK, APLOG_NOERRNO|APLOG_WARNING, 0, NULL,
                   "%s: empty argument #%d", where, i + 1);
    }
  }
#endif
}

/******************************************************** SUBSTITUTION UTILS */

/* could be switched to '\'' */
#define DELIM '"'
#define ESCAPE '\\'

/* returns the number of needed escapes for the string
 */
static int number_of_escapes(const char delim, const char * str)
{
  int nesc = 0;
  const char * s = str;
  while (*s) {
    if (*s == ESCAPE || *s == delim)
      nesc++;
    s++;
  }
  debug(fprintf(stderr, "escapes: %d ---%s---\n", nesc, str));
  return nesc;
}

/* replace name by replacement at the beginning of buf of bufsize.
   returns an error message or NULL.
*/
static char * substitute(
  char * buf, int bufsize,
  const char * name,
  const char * replacement,
  mm_boolean do_esc)
{
  int
    lbuf  = strlen(buf),
    lname = strlen(name),
    lrepl = strlen(replacement),
    lsubs = lrepl + (do_esc? (2+number_of_escapes(DELIM, replacement)): 0),
    shift = lsubs - lname,
    size  = lbuf + shift,
    i, j;

    /* buf must starts with name */
  ap_assert(!strncmp(buf, name, lname));

  /* ??? */
  if (!strcmp(name,replacement)) return NULL;

  debug(fprintf(stderr,
                "substitute(%s,%s,%s,%d,sh=%d,lbuf=%d,lrepl=%d,lsubs=%d)\n",
                buf,name,replacement, do_esc, shift, lbuf, lrepl, lsubs));

  if (size >= bufsize) {
    /* could/should I reallocate? */
    return "cannot substitute, buffer size too small";
  }

  /* shift the end of line */
  if (shift < 0) {
    for (i = lname; i <= lbuf; i++)
      buf[i + shift] = buf[i];
  } else if (shift > 0) {
    for (i = lbuf; i >= lname; i--)
      buf[i + shift] = buf[i];
  }

  /* insert the replacement with escapes */
  j = 0;
  if (do_esc) buf[j++] = DELIM;
  for (i = 0; i < lrepl; i++, j++) {
    if (do_esc && (replacement[i] == DELIM || replacement[i] == ESCAPE))
      buf[j++] = ESCAPE;
    buf[j] = replacement[i];
  }
  if (do_esc) buf[j++] = DELIM;

  return NULL;
}

/* find first occurence of args in buf.
   in case of conflict, the LONGEST argument is kept. (could be the FIRST?).
   returns the pointer and the whichone found, or NULL.
*/
static char * next_substitution(
  const char * buf,
  const apr_array_header_t * args,
  int * whichone)
{
  int i;
  char * chosen = NULL, * found, ** tab = (char **)args->elts;
  size_t lchosen = 0, lfound;

  for (i = 0; i < args->nelts; i++) {
    found = ap_strstr(buf, tab[i]);
    lfound = strlen(tab[i]);
    if (found && (!chosen || found < chosen ||
                  (found == chosen && lchosen < lfound))) {
      chosen = found;
      lchosen = lfound;
      *whichone = i;
    }
  }

  return chosen;
}

/* substitute macro arguments by replacements in buf of bufsize.
   returns an error message or NULL.
   if used is defined, returns the used macro arguments.
*/
static char * substitute_macro_args(
  char * buf, int bufsize,
  const macro_t * macro,
  const apr_array_header_t * replacements,
  apr_array_header_t * used)
{
  char * ptr = buf, * errmsg,
    ** atab = (char **)macro->arguments->elts,
    ** rtab = (char **)replacements->elts;
  int whichone = -1;

  if (used) {
    ap_assert(used->nalloc >= replacements->nelts);
  }
  debug(fprintf(stderr, "1# %s", buf));

  while ((ptr = next_substitution(ptr, macro->arguments, &whichone))) {
    errmsg = substitute(ptr, buf - ptr + bufsize,
                        atab[whichone], rtab[whichone],
                        atab[whichone][0]==ESCAPE_ARG);
    if (errmsg) {
      return errmsg;
    }
    ptr += strlen(rtab[whichone]);
    if (used) {
      used->elts[whichone] = 1;
    }
  }
  debug(fprintf(stderr, "2# %s", buf));

  return NULL;
}

/* perform substitutions in a macro contents and
   return the result as a newly allocated array, if result is defined.
   may also return an error message.
   passes used down to substitute_macro_args.
*/
static const char * process_content(
  apr_pool_t * p,
  const macro_t * macro,
  const apr_array_header_t * replacements,
  apr_array_header_t * used,
  apr_array_header_t ** result)
{
  apr_array_header_t * contents = macro->contents;
  char ** new, * errmsg, line[MAX_STRING_LEN];
  int i;

  if (result) {
    *result = apr_array_make(p, 1, sizeof(char *));
  }

  for (i = 0; i < contents->nelts; i++) {
    strncpy(line, ((char **)contents->elts)[i], MAX_STRING_LEN - 1);
    errmsg = substitute_macro_args(line, MAX_STRING_LEN,
                                   macro, replacements, used);
    if (errmsg) {
      return apr_psprintf(p, "while processing line %d of macro '%s'"
                          " (%s)%s",
                          i + 1, macro->name, macro->location, errmsg);
    }

    if (result) {
      new = apr_array_push(*result);
      *new = apr_pstrdup(p, line);
    }
  }
  return NULL;
}

/* warn if some macro arguments are not used.
 */
static const char * check_macro_contents(apr_pool_t * p, const macro_t * macro)
{
#if !defined(MOD_MACRO_NO_WARNINGS)

  int nelts = macro->arguments->nelts, i;
  apr_array_header_t * used;
  const char * errmsg;
  char ** names = (char **)macro->arguments->elts;

  if (macro->contents->nelts == 0) {
    ap_log_error(APLOG_MARK, APLOG_NOERRNO|APLOG_WARNING, 0, NULL,
                 "macro '%s' (%s): empty contents!",
                 macro->name, macro->location);
    return NULL; /* no need to further warnings... */
  }

  used = apr_array_make(p, nelts, sizeof(char));

  for (i = 0; i < nelts; i++) {
    used->elts[i] = 0;
  }

  errmsg = process_content(p, macro, macro->arguments, used, NULL);

  if (errmsg) {
    /* free used. */
    return errmsg;
  }

  for (i = 0; i < nelts; i++) {
    if (!used->elts[i]) {
      ap_log_error(APLOG_MARK, APLOG_NOERRNO|APLOG_WARNING, 0, NULL,
                   "macro '%s' (%s): argument '%s' (#%d) never used",
                   macro->name, macro->location, names[i], i + 1);
    }
  }

    /* free used. */
#endif

  return NULL;
}

/********************************************************* MACRO CONFIG FILE */

/* the expanded content of the macro is to be parsed as a ap_configfile_t.
   the following struct stores the content.


   IMPORTANT NOTE:
   ---------------

   in http_config.c there is such a stuff made static,
   which does not implement getch().
   maybe this should be moved to util.c ???
*/
typedef struct {
  int index;        /* current element. */
  int char_index;       /* current char in element. */
  int length;             /* cached length of the current line. */
  apr_array_header_t * contents;/* array of char * */
  ap_configfile_t * next;       /* next config once this one is processed. */
  ap_configfile_t ** upper;     /* hack: where to update it if needed. */
} array_contents_t;

/* next config if any. */
static mm_boolean next_one(array_contents_t * ml)
{
  if (ml->next) {
    ap_assert(ml->upper);
    *(ml->upper) = ml->next;
    return mm_true;
  }
  return mm_false;
}

/* returns next char or -1.
 */
static int array_getch(void * param)
{
  array_contents_t * ml = (array_contents_t *)param;
  char ** tab = (char **)ml->contents->elts;

  while (ml->char_index >= ml->length) { /* next element */
    if (ml->index >= ml->contents->nelts) {
      /* maybe update. */
      if (ml->next && ml->next->getch && next_one(ml)) {
        return ml->next->getch(ml->next->param);
      }
      return -1;
    }
    ml->index++;
    ml->char_index = 0;
    ml->length = ml->index >= ml->contents->nelts? 0: strlen(tab[ml->index]);
  }

  return tab[ml->index][ml->char_index++];
}

/* returns a buf a la fgets.
   no more than a line at a time, otherwise the parsing is too much ahead...
   NULL at EOF.
*/
static void * array_getstr(void * buf, size_t bufsize, void * param)
{
  array_contents_t * ml = (array_contents_t *)param;
  char * buffer = (char *) buf;
  size_t i = 0;
  int next = 0;

  while (i < bufsize - 1 && next != '\n'
         && ((next = array_getch(param)) != -1)) {
    buffer[i++] = (char)next;
  }

  if (next == -1 && i == 0) { /* EOF */
    /* maybe update to next. */
    if (next_one(ml)) {
      ap_assert(ml->next->getstr);
      return ml->next->getstr(buf, bufsize, ml->next->param);
    }
    return NULL;
  }

  buffer[i] = '\0';
  return buf;
}

/* close the array stream?
 */
static int array_close(void * param)
{
  array_contents_t * ml = (array_contents_t *)param;
  ml->index = ml->contents->nelts;
  ml->char_index = ml->length;
  return 0;
}

/* this one could be exported.
 */
static ap_configfile_t * make_array_config(
  apr_pool_t * p,
  apr_array_header_t * contents,
  const char * where,
  ap_configfile_t * cfg,
  ap_configfile_t ** upper)
{
  array_contents_t * ls =
    (array_contents_t *)apr_palloc(p, sizeof(array_contents_t));

  ls->index      = 0;
  ls->char_index = 0;
  ls->contents   = contents;
  ls->length     = ls->contents->nelts < 1
    ? 0 : strlen(((char **)ls->contents->elts)[0]);
  ls->next       = cfg;
  ls->upper      = upper;

  return ap_pcfg_open_custom(p, where, (void *)ls,
                             array_getch, array_getstr, array_close);
}


/********************************************************** KEYWORD HANDLING */

/* handles: <Macro macroname arg1 arg2 ...> any trash...
 */
static const char *macro_section(
  cmd_parms * cmd,
  void * dummy,
  const char * arg)
{
  const char * errmsg, * where;
  char ** new, * name, * endp;
  macro_t * macro, * old;

  debug(fprintf(stderr, "macro_section: arg='%s'\n", arg));

  macro_init(cmd->temp_pool); /* lazy... */

  endp = ap_strrchr_c(arg, '>');
  if (endp == NULL) {
    return apr_pstrcat(cmd->pool, cmd->cmd->name,
                       "> directive missing closing '>'", NULL);
  }

  /* hmmm... drops out '>[^>]*$'
   */
  if (endp) {
    *endp = '\0';
  }

  /* get name. */
  name = ap_getword_conf(cmd->temp_pool, &arg);

  if (empty_string_p(name)) {
    return "macro definition: name not specified";
  }

  old = get_macro_by_name(all_macros, name);
  if (old) {
#if !defined(MOD_MACRO_NO_WARNINGS)
    /* already define: warn about the redefinition. */
    ap_log_error(APLOG_MARK, APLOG_NOERRNO|APLOG_WARNING, 0, NULL,
                 "macro '%s' multiply defined. "
                 "%s, redefined on line %d of %s",
                 old->name, old->location,
                 cmd->config_file->line_number, cmd->config_file->name);
#endif
    macro = old;
  }
  else {
    macro = (macro_t *)apr_palloc(cmd->temp_pool, sizeof(macro_t));
  }

  macro->name = name;
  debug(fprintf(stderr, "macro_section: name=%s\n", name));

  /* get arguments. */
  macro->location = apr_psprintf(cmd->temp_pool,
                                 "defined on line %d of %s",
                                 cmd->config_file->line_number,
                                 cmd->config_file->name);
  debug(fprintf(stderr, "macro_section: location=%s\n", macro->location));

  where = apr_psprintf(cmd->temp_pool, "macro '%s' (%s)",
                       macro->name, macro->location);

#if !defined(MOD_MACRO_NO_CHAR_PREFIX_WARNINGS) ||  \
    !defined(MOD_MACRO_NO_WARNINGS)
  if (looks_like_an_argument(name)) {
    ap_log_error(APLOG_MARK, APLOG_NOERRNO|APLOG_WARNING, 0, NULL,
                 "%s better prefix a macro name with any of '%s'\n",
                 where, ARG_PREFIX);
  }
#endif

  macro->arguments = get_arguments(cmd->temp_pool, arg);

  errmsg = check_macro_arguments(cmd->temp_pool, macro);

  if (errmsg) {
    return errmsg;
  }

  errmsg = get_lines_till_end_token(cmd->temp_pool, cmd->config_file,
                                    END_MACRO, BEGIN_MACRO,
                                    where, &macro->contents);

  if (errmsg) {
    return apr_psprintf(cmd->temp_pool,
                        "%s\n\tcontents error: %s", where, errmsg);
  }

  errmsg = check_macro_contents(cmd->temp_pool, macro);

  if (errmsg) {
    return apr_psprintf(cmd->temp_pool,
                        "%s\n\tcontents checking error: %s", where, errmsg);
  }

  /* add the new macro. */
  new  = apr_array_push(all_macros);
  *new = (char *)macro;

  return NULL;
}

/* handles: Use name value1 value2 ...
 */
static const char * use_macro(cmd_parms * cmd, void * dummy, const char * arg)
{
  char * name, * where = "", * recursion;
  const char * errmsg;
  apr_array_header_t * contents, * replacements;
  macro_t * macro;

  debug(fprintf(stderr, "use_macro -%s-\n", arg));

  macro_init(cmd->temp_pool); /* lazy... */

  name = ap_getword_conf(cmd->temp_pool, &arg);

  if (empty_string_p(name)) {
    return "no macro name specified in " USE_MACRO;
  }

  macro = get_macro_by_name(all_macros, name);

  if (!macro) {
    return apr_psprintf(cmd->temp_pool, "macro '%s' is not defined", name);
  }

  /* recursion is detected by looking at the config file name,
     which may already contains "macro 'foo'". Ok, it looks like a hack,
     but otherwise it is uneasy to keep this data available somewhere...
     the name has just the needed visibility and liveness.
  */
  recursion = apr_pstrcat(cmd->temp_pool, "macro '", macro->name, "'", NULL);

  if (ap_strstr(cmd->config_file->name, recursion)) {
    return apr_psprintf(cmd->temp_pool,
                        "%s: recursive use of macro '%s' is invalid.",
                        where, macro->name);
  }

  replacements = get_arguments(cmd->temp_pool, arg);

  if (macro->arguments->nelts != replacements->nelts) {
    return apr_psprintf(cmd->temp_pool,
                        "use of macro '%s' %s "
                        " with %d arguments instead of %d",
                        macro->name, macro->location, replacements->nelts,
                        macro->arguments->nelts);
  }

  where = apr_psprintf(cmd->temp_pool,
                       "macro '%s' (%s) used on line %d of %s",
                       macro->name, macro->location,
                       cmd->config_file->line_number,
                       cmd->config_file->name);

  check_macro_use_arguments(where, replacements);

  errmsg = process_content(cmd->temp_pool, macro, replacements,
                           NULL, &contents);

  if (errmsg) {
    return apr_psprintf(cmd->temp_pool,
                        "%s error while substituting:\n%s",
                        where, errmsg);
  }

  /* fix??? why is it wrong? should I -- the new one? */
  cmd->config_file->line_number++;

  cmd->config_file = make_array_config
    (cmd->temp_pool, contents, where, cmd->config_file, &cmd->config_file);

  return NULL;
}

/************************************ ERROR AND WARNING DURING CONFIGURATION */

/* maybe ConfigurationError and ConfigurationWarning could be used?
 */
#define ERROR_KEYWORD "Error"
#define WARNING_KEYWORD "Warning"

/* configuration generated erros or warnings.
 */
static const char * say_it(cmd_parms * parms, void * dummy, const char * arg)
{
  int level = (int)parms->info;
  /* for some obscure reason, since apache 2.2
   * the config_file pointer is sometimes NULL.
   * that is also true of the err_directive stuff.
   */
  ap_configfile_t * cf = parms->config_file;
  ap_directive_t const * ed1 = parms->directive;
  ap_directive_t const * ed2 = parms->err_directive;
  trim(arg);

  ap_log_error(APLOG_MARK, APLOG_NOERRNO|level, 0, NULL,
               "on line %d of %s: %s",
               /* pick up the data somewhere... */
               cf? cf->line_number:
               ed1? ed1->line_num:
               ed2? ed2->line_num: -1,
               cf? cf->name:
               ed1? ed1->filename:
               ed2? ed2->filename: "<NULL>", arg);

  return level & APLOG_ERR ?
    "configuration file processing aborted by " ERROR_KEYWORD "." : NULL;
}

/************************************************************* EXPORT MODULE */

/* macro module commands.
 */
static const command_rec macro_cmds[] =
{
  /* configuration file macro stuff
   */
  AP_INIT_RAW_ARGS(BEGIN_MACRO, macro_section, NULL, EXEC_ON_READ | OR_ALL,
       "Beginning of a macro definition section."),
  AP_INIT_RAW_ARGS(USE_MACRO, use_macro, NULL, EXEC_ON_READ | OR_ALL,
       "Use of a macro."),

  /* configuration errors and warnings.
   */
  AP_INIT_RAW_ARGS(ERROR_KEYWORD, say_it, (void *)APLOG_ERR, OR_ALL,
       "Error in a configuration file."),
  AP_INIT_RAW_ARGS(WARNING_KEYWORD, say_it, (void *)APLOG_WARNING, OR_ALL,
       "Warning in a configuration file."),

  { NULL }
};

/* Module hooks are request-oriented thus it does not suit configuration
   file utils a lot. I haven't found any clean hook to apply something
   before then after configuration file processing. Also what about
   .htaccess files?

   Thus I think that main/http_co*.c would be a better place for this stuff.
*/

AP_DECLARE_DATA module macro_module = {
  STANDARD20_MODULE_STUFF,
  NULL,   /* create per-directory config  */
  NULL,   /* merge per-directory config structures */
  NULL,   /* create per-server config structure */
  NULL,   /* merge per-server config structures */
  macro_cmds,   /* command table */
  NULL    /* register hooks */
};
