// Tokenize the SQL input stream into a series of tokens to feed into the parser.
// Original sql tokenizer:google3/storage/googlesql/parser/flex_tokenizer.l
// Original sql parser: google3/storage/googlesql/parser/bison_parser.y

language sql(ts);

eventBased = true
genSelector = true
debugParser = false // Set to true to print the parser shift/reduce decisions.
fixWhitespace = true
fileNode = "File"
optimizeTables = true
scanBytes = true
caseInsensitive = true
tokenStream = true

:: lexer

//// Sets of active rules a.k.a "start conditions"
//
// All lexer rules are have a start condition. When not set explicitly, the start
// condition is `initial`.
//
// Either the set of active rules excludes or includes the `initial` rules:
// - exclusive: only the rules explicitly marked with an exclusive start
//   condition are active when that start condition is set.
//   Rules in an exclusive start condition constitute a small separate
//   tokenizer. Declared with %x.
//
// - inclusive: the `initial` rules are also active when an inclusive start
//   condition is set. Declared with %s.
//
// Note: lexer rules marked with start condition * are active whichever
// start condition is set.

/* After "." we allow more things, including all keywords and all
   integers, to be returned as identifiers. This state is initiated when we
   recognize an identifier followed by a ".". It is also initiated after a
   closing parenthesis, square bracket, or "?" (positional parameter) followed
   by a ".", to handle cases like foo[3].array. See the "." rule and the
   dotIdentifier{generalized_identifier}.
*/
%x dotIdentifier;

/* This inclusive state is for in ARRAY<...>, STRUCT<...>, and RANGE<...>. It
   turns off the parsing of <<, >>, and <>, but leaves everything else the same.
   Doing this in the tokenizer avoids complicated rules and duplication at the
   parser level.
*/
%s inTemplatedType;

/* This inclusive state is for BETWEEN...AND. In this state, everything works
   as normal, but the "AND" keyword is returned as AND_FOR_BETWEEN instead of
   KW_AND. This resolves what would otherwise be ambiguous in the bison grammar.
   Note that this state is automatically turned off within parentheses and
   square brackets, using l.push() and l.pop(). That ensures that
   something like BETWEEN (a AND b) AND c still parses.
*/
%s inBetween;

//// Named patterns
// They are declared with `=`, while rules are declared with `:`.
// Named patterns are only useable on the right hand side of lexer rules and
// other named patterns: they can't be used in parser rules, and can't be
// reported as tokens.

// These are some basic regex definitions that are used in the lexer rules below.
decimal_digit       =      /[0-9]/
decimal_digits      =      /{decimal_digit}+/
hex_digit           =      /[0-9a-f]/
hex_integer         =      /(0x{hex_digit}+)/

dot                 =      /\./
exp_nosign          =      /e{decimal_digits}/
exp_sign            =      /e[+-]{decimal_digits}/
exp                 =      /({exp_nosign}|{exp_sign})/

/* Floating point formats are identified by the presence of a dot and/or an
   exponent. If there's a dot, there has to be at least one digit either before
   or after the dot. This is covered by the first two regexes. The third regex
   covers digits with an exponent but without a dot. */
decimal_dot             =    /{decimal_digits}{dot}{decimal_digits}?{exp}?/
dot_decimal             =    /{dot}{decimal_digits}{exp}?/
decimal_exp             =    /{decimal_digits}{exp}/
floating_point_literal  =    /{decimal_dot}|{dot_decimal}|{decimal_exp}/

/* Whitespace, including Unicode whitespace characters encoded as UTF-8, as well
   as all comments.
   https://www.cs.tut.fi/~jkorpela/chars/spaces.html
*/
utf8_no_break_space            = /\u00A0/
utf8_en_quad                   = /\u2000/
utf8_em_quad                   = /\u2001/
utf8_en_space                  = /\u2002/
utf8_em_space                  = /\u2003/
utf8_three_per_em_space        = /\u2004/
utf8_four_per_em_space         = /\u2005/
utf8_six_per_em_space          = /\u2006/
utf8_figure_space              = /\u2007/
utf8_punctuation_space         = /\u2008/
utf8_thin_space                = /\u2009/
utf8_hair_space                = /\u200A/
utf8_narrow_no_break_space     = /\u202F/
utf8_medium_mathematical_space = /\u205F/
utf8_ideographic_space         = /\u3000/
whitespace_character           = /([ \n\r\t\x08\f\v]|{utf8_no_break_space}|{utf8_en_quad}|{utf8_em_quad}|{utf8_en_space}|{utf8_em_space}|{utf8_three_per_em_space}|{utf8_four_per_em_space}|{utf8_six_per_em_space}|{utf8_figure_space}|{utf8_punctuation_space}|{utf8_thin_space}|{utf8_hair_space}|{utf8_narrow_no_break_space}|{utf8_medium_mathematical_space}|{utf8_ideographic_space})/
opt_whitespace                 = /({whitespace_character}|{comment_w_end_of_line})*/
whitespace_no_comments         = /{whitespace_character}+/

/* String/bytes literals and identifiers.

   The abbreviations here:
     sq = single quote(d)
     dq = double quote(d)
     bq = back quote(d)
     3 = triple quoted
     r = raw
     _0 = unterminated versions. They are used to return better error
          messages for unterminated strings.

   For instance, rsq3 means 'raw triple single-quoted', or r'''...'''.
*/
any_escape              =  /(\\(.|\n|\r|\r\n))/
sq                      =  /'/
sq3                     =  /'''/
dq                      =  /"/
dq3                     =  /"""/
bq                      =  /`/
no_backslash_sq_newline =  /[^'\\\n\r]/
no_backslash_dq_newline =  /[^"\\\n\r]/
no_backslash_sq         =  /[^'\\]/
no_backslash_dq         =  /[^"\\]/


/* Strings and bytes: */
sqtext_0       =    /{sq}({no_backslash_sq_newline}|{any_escape})*/
sqtext         =    /{sqtext_0}{sq}/
dqtext_0       =    /{dq}({no_backslash_dq_newline}|{any_escape})*/
dqtext         =    /{dqtext_0}{dq}/
sq3text_0      =    /{sq3}(({sq}|{sq}{sq})?({no_backslash_sq}|{any_escape}))*/
sq3text        =    /{sq3text_0}{sq3}/
dq3text_0      =    /{dq3}(({dq}|{dq}{dq})?({no_backslash_dq}|{any_escape}))*/
dq3text        =    /{dq3text_0}{dq3}/
bytes_literal               =    /(b|rb|br)({sqtext}|{dqtext}|{sq3text}|{dq3text})/
unterminated_string_literal =    /({sqtext_0}|{dqtext_0})/
unterminated_triple_quoted_string_literal = /({sq3text_0}|{dq3text_0})/
unterminated_raw_string_literal = /r({sqtext_0}|{dqtext_0})/
unterminated_triple_quoted_raw_string_literal = /r({sq3text_0}|{dq3text_0})/
unterminated_bytes_literal   =   /b({sqtext_0}|{dqtext_0})/
unterminated_triple_quoted_bytes_literal = /b({sq3text_0}|{dq3text_0})/
unterminated_raw_bytes_literal =  /(rb|br)({sqtext_0}|{dqtext_0})/
unterminated_triple_quoted_raw_bytes_literal = /(rb|br)({sq3text_0}|{dq3text_0})/


/* Identifiers: */
unquoted_identifier             = /[A-Z_][A-Z_0-9]*/
unquoted_generalized_identifier = /[A-Z_0-9]+/
bqtext_0                        = /{bq}([^\\`\r\n]|({any_escape}))*/
bqtext                          = /{bqtext_0}{bq}/
identifier                      = /{unquoted_identifier}|{bqtext}/
generalized_identifier          = /{unquoted_generalized_identifier}|{bqtext}/
unterminated_escaped_identifier = /{bqtext_0}/


/* C-style comments using slash+star.
   cs_ prefix is for 'c-style comment', shortened to avoid long lines.
   For more information about how this works, see
   'Using one, even more complicated, pattern' from
   http://www.cs.man.ac.uk/~pjj/cs212/ex2_str_comm.html
*/

cs_start             = /\/\*/
cs_not_star          = /[^\*]/
cs_star              = /\*/
cs_not_star_or_slash = /[^\/\*]/
cs_slash             = /\//

/* Contents of a C-style comment that may embed a * (or a sequence of stars)
   followed by not-a-slash. */
cs_embed_star        = /({cs_not_star}*({cs_star}+{cs_not_star_or_slash})*)*/
/* Matches the beginning of a comment, to detect unterminated comments. */
cs_comment_begin     = /{cs_start}{cs_embed_star}{cs_star}*/
cs_comment           = /{cs_start}{cs_embed_star}{cs_star}+{cs_slash}/

/* comments with double dashes prefix `--` */
dash_comment         = /\-\-.*/
dash_comment_w_end_of_line = /\-\-[^\r\n]*(\r|\n|\r\n)/

/* comments with with pound prefix `#` */
pound_comment        = /#.*/
pound_comment_w_end_of_line        = /#[^\r\n]*(\r|\n|\r\n)/

/* comments with with double slash prefix `//` */
/* note: not part of standard GoogleSQL, but allowed because Plx allows it .*/
slash_comment        = /\/\/.*/
slash_comment_w_end_of_line        = /\/\/[^\r\n]*(\r|\n|\r\n)/

line_comment          = /({dash_comment}|{pound_comment}|{slash_comment})/
comment_w_end_of_line =  /({cs_comment}|{dash_comment_w_end_of_line}|{pound_comment_w_end_of_line}|{slash_comment_w_end_of_line})/

// Token below is part of the syntax of the RUN statement. This token is missing
// from bison_parser, so it is outside the generated window.
'::': /::/

// begin window of tokens generated from bison_parser.y


// begin other


// begin keyword

// A Token, whose name matches exactly the source that represents it,
// is surrounded with single quote. Examples:
// 1. token `'BETWEEN'` matches exactly BETWEEN in SQL.
// 2. token `AND_FOR_BETWEEN` has no single quotes, and it only match `and`.
//
// There is a lexer conflict between keyword tokens and identifier_in_lexer.
// The latter uses -1 as priority, so individual keyword tokens are preferred.
// https://textmapper.org/documentation.html//lexer-conflicts
'ALL': /all/
<inBetween> AND_FOR_BETWEEN: /and/  {
    this.pop();
}
<initial> 'AND' (KW_AND): /and/
'ANY': /any/
'ARRAY': /array/
'AS': /as/
'ASC': /asc/
'ASSERT_ROWS_MODIFIED': /assert_rows_modified/
'AT': /at/
'BETWEEN': /between/  {
     this.push(StateInBetween);
}
'BY': /by/
'CASE': /case/
'CAST': /cast/
'COLLATE': /collate/
'CONTAINS': /contains/
'CREATE': /create/
'CROSS': /cross/
'CUBE': /cube/
'CURRENT': /current/
'DEFAULT': /default/
'DEFINE': /define/
'DEPTH': /depth/
'DESC': /desc/
'DISTINCT': /distinct/
'ELSE': /else/
'END': /end/
'ENUM': /enum/
'ESCAPE': /escape/
'EXCEPT': /except/  // must be followed by '(', all, distinct, open_hint, or open_integer_hint
'EXCLUDE': /exclude/
'EXISTS': /exists/
'EXTRACT': /extract/
'FALSE': /false/
'FETCH': /fetch/
'FOLLOWING': /following/
'FOR': /for/
'FROM': /from/
'FULL': /full/
FULL_IN_SET_OP:  // 'full' followed by `outer? (union | intersect | except)`
'GRAPH_TABLE': /graph_table/
'GROUP': /group/
'GROUPING': /grouping/
'GROUPS': /groups/
'HASH': /hash/
'HAVING': /having/
'IF': /if/
'IGNORE': /ignore/
'IN': /in/
'INNER': /inner/
'INTERSECT': /intersect/
'INTERVAL': /interval/
'INTO': /into/
'IS': /is/
'JOIN': /join/
'LATERAL': /lateral/
'LEFT': /left/
LEFT_IN_SET_OP:   // 'left' followed by `outer? (union | intersect | except)`
'LIKE': /like/
'LIMIT': /limit/
'LOOKUP': /lookup/
'MERGE': /merge/
'NATURAL': /natural/
'NEW': /new/
'NO': /no/
'NOT': /not/
/* This returns a different token because returning KW_NOT would confuse the
    operator precedence parsing. Boolean NOT has a different precedence than
    NOT BETWEEN/IN/LIKE/DISTINCT. The final character at the end is intended to avoid
    cases like "NOT Info.foo" being interpreted as having a NOT for IN. This
    unfortunately doesn't match at EOF, so "NOT IN" at the very end of the file
    will cause bad error messages. There is no situation where that is valid
    syntax, so there will never be any rejections as a result.
 */// If `not` is followed by one of `between`, `in` and `like` followed by a space
// or [^a-zA-Z_0-9] then it matches this rule. But this rule will only consume
// the `not` string.
NOT_SPECIAL:
'NULL': /null/
'NULLS': /nulls/
'OF': /of/
'ON': /on/
KW_OR: /or/
'ORDER': /order/
'OUTER': /outer/
'OVER': /over/
'PARTITION': /partition/
'PRECEDING': /preceding/
'PROTO': /proto/
'QUALIFY': /qualify/
'RANGE': /range/
'RECURSIVE': /recursive/
'RESPECT': /respect/
'RIGHT': /right/
'ROLLUP': /rollup/
'ROWS': /rows/
'SELECT': /select/
'SET': /set/
'SOME': /some/
'STRUCT': /struct/
'TABLESAMPLE': /tablesample/
'THEN': /then/
'TO': /to/
'TREAT': /treat/
'TRUE': /true/
'UNBOUNDED': /unbounded/
'UNION': /union/
'UNNEST': /unnest/
'USING': /using/
'WHEN': /when/
'WHERE': /where/
'WINDOW': /window/
'WITH': /with/
'WITHIN': /within/
WITH_EXPR: // 'with' followed by a '('

// begin softkeyword

'ABORT': /abort/
'ACCESS': /access/
'ACTION': /action/
'ADD': /add/
'AGGREGATE': /aggregate/
'ALTER': /alter/
'ALWAYS': /always/
'ANALYZE': /analyze/
'APPROX': /approx/
'ARE': /are/
'ASCENDING': /ascending/
'ASSERT': /assert/
'BATCH': /batch/
'BEGIN': /begin/
'BIGDECIMAL': /bigdecimal/
'BIGNUMERIC': /bignumeric/
'BREAK': /break/
'CALL': /call/
'CASCADE': /cascade/
'CHECK': /check/
'CLAMPED': /clamped/
'CLONE': /clone/
'CLUSTER': /cluster/
'COLUMN': /column/
'COLUMNS': /columns/
'COMMIT': /commit/
'CONNECTION': /connection/
'CONSTANT': /constant/
'CONSTRAINT': /constraint/
'CONTINUE': /continue/
'COPY': /copy/
'CORRESPONDING': /corresponding/
'CYCLE': /cycle/
'DATA': /data/
'DATABASE': /database/
'DATE': /date/
'DATETIME': /datetime/
'DECIMAL': /decimal/
'DECLARE': /declare/
'DEFINER': /definer/
'DELETE': /delete/
'DELETION': /deletion/
'DESCENDING': /descending/
'DESCRIBE': /describe/
'DESCRIPTOR': /descriptor/
'DESTINATION': /destination/
'DETERMINISTIC': /deterministic/
'DO': /do/
'DROP': /drop/
'EDGE': /edge/
'ELSEIF': /elseif/
'ENFORCED': /enforced/
KW_ERROR: /error/
'EXCEPTION': /exception/
EXCEPT_IN_SET_OP: // EXCEPT that is followed by a hint, ALL or DISTINCT.
'EXECUTE': /execute/
'EXPLAIN': /explain/
'EXPORT': /export/
'EXTEND': /extend/
'EXTERNAL': /external/
'FILES': /files/
'FILL': /fill/
'FILTER': /filter/
'FIRST': /first/
'FOREIGN': /foreign/
'FORMAT': /format/
'FUNCTION': /function/
'GENERATED': /generated/
'GRANT': /grant/
'GRAPH': /graph/
'GROUP_ROWS': /group_rows/
'HIDDEN': /hidden/
'IDENTITY': /identity/
'IMMEDIATE': /immediate/
'IMMUTABLE': /immutable/
'IMPORT': /import/
'INCLUDE': /include/
'INCREMENT': /increment/
'INDEX': /index/
'INOUT': /inout/
'INPUT': /input/
'INSERT': /insert/
'INTERLEAVE': /interleave/         // Spanner-specific keyword
'INVOKER': /invoker/
'ISOLATION': /isolation/
'ITERATE': /iterate/
'JSON': /json/
'KEY': /key/
'LABEL': /label/
'LANGUAGE': /language/
'LAST': /last/
'LEAVE': /leave/
'LET': /let/
'LEVEL': /level/
'LOG': /log/
'LOAD': /load/
'LOOP': /loop/
'MACRO': /macro/
'MATCH': /match/
'MATCHED': /matched/
'MATERIALIZED': /materialized/
'MAX': /max/
'MAXVALUE': /maxvalue/
'MESSAGE': /message/
'METADATA': /metadata/
'MIN': /min/
'MINVALUE': /minvalue/
'MODEL': /model/
'MODULE': /module/
'NEXT': /next/
'NODE': /node/
'NULL_FILTERED': /null_filtered/   // Spanner-specific keyword
'NUMERIC': /numeric/
'OFFSET': /offset/
'ONLY': /only/
'OPTIONAL': /optional/
'OPTIONS': /options/
'OUT': /out/
'OUTPUT': /output/
'OVERWRITE': /overwrite/
'PARENT': /parent/                 // Spanner-specific keyword
'PARTITIONS': /partitions/
'PERCENT': /percent/
'PIVOT': /pivot/
'POLICIES': /policies/
'POLICY': /policy/
'PRIMARY': /primary/
'PRIVATE': /private/
'PRIVILEGE': /privilege/
'PRIVILEGES': /privileges/
'PROCEDURE': /procedure/
'PROJECT': /project/
'PROPERTIES': /properties/
'PROPERTY': /property/
'PUBLIC': /public/
// TODO(b/263109796): support language options and conditionally treat qualify as non-reserved
QUALIFY_NONRESERVED:
'RAISE': /raise/
'READ': /read/
'REFERENCES': /references/
'REMOTE': /remote/
'REMOVE': /remove/
'RENAME': /rename/
'REPEAT': /repeat/
'REPEATABLE': /repeatable/
'REPLACE': /replace/
'REPLACE_FIELDS': /replace_fields/
'REPLICA': /replica/
'REPORT': /report/
'RESTRICT': /restrict/
'RESTRICTION': /restriction/
'RETURN': /return/
'RETURNS': /returns/
'REVOKE': /revoke/
'ROLLBACK': /rollback/
'ROW': /row/
'RUN': /run/
'SAFE_CAST': /safe_cast/
'SCHEMA': /schema/
'SEARCH': /search/
'SECURITY': /security/
'SEQUENCE': /sequence/   // Note: becomes identifier in the parser when followed by CLAMPED.
'SETS': /sets/
'SHOW': /show/
'SIMPLE': /simple/
'SKIP': /skip/
'SNAPSHOT': /snapshot/
'SOURCE': /source/
'SQL': /sql/
'STABLE': /stable/
'START': /start/
'STATIC_DESCRIBE': /static_describe/
'STORED': /stored/
'STORING': /storing/
'STRICT': /strict/
'SYSTEM': /system/
'SYSTEM_TIME': /system_time/
'TABLE': /table/
'TABLES': /tables/
'TARGET': /target/
'TEMP': /temp/
'TEMPORARY': /temporary/
'TIME': /time/
'TIMESTAMP': /timestamp/
'TRANSACTION': /transaction/
'TRANSFORM': /transform/
'TRUNCATE': /truncate/
'TYPE': /type/
'UNDROP': /undrop/
'UNIQUE': /unique/
'UNKNOWN': /unknown/
'UNPIVOT': /unpivot/
'UNTIL': /until/
'UPDATE': /update/
'VALUE': /value/
'VALUES': /values/
'VECTOR': /vector/
'VIEW': /view/
'VIEWS': /views/
'VOLATILE': /volatile/
'WEIGHT': /weight/
'WHILE': /while/
'WRITE': /write/
'ZONE': /zone/

// begin literal

script_label: /{identifier}{opt_whitespace}[:]{opt_whitespace}(begin|while|loop|repeat|for)/ {
    this.rewind(backupOffset)  // assuming we will backtrack to "identifier_in_lexer"
}

// TODO(jdbrowne): support empty lines in the differ to allow grouping invalid
// tokens with the literal they are designed for.
// TODO(jdbrowne): support end of line comments with an "epilogue" and use the
// edit AST to detect end-of-line comments

string_literal: /r?({sqtext}|{dqtext}|{sq3text}|{dq3text})/
invalid_token: /{unterminated_string_literal}/
invalid_token: /{unterminated_triple_quoted_string_literal}/
invalid_token: /{unterminated_raw_string_literal}/
invalid_token: /{unterminated_triple_quoted_raw_string_literal}/
bytes_literal: /{bytes_literal}/
invalid_token: /{unterminated_bytes_literal}/
invalid_token: /{unterminated_triple_quoted_bytes_literal}/
invalid_token: /{unterminated_raw_bytes_literal}/
invalid_token: /{unterminated_triple_quoted_raw_bytes_literal}/
integer_literal: /{decimal_digits}/
integer_literal: /{hex_integer}/
floating_point_literal: /{floating_point_literal}/ {
    if (this.isAtDotAfterClosingBracketOrSoftKeyword()) {
      this.rewind(this._tokenOffset + 1);
      this.push(StateDotIdentifier);
      tok = token.TokenType.DOT;
    }
}
/* Error rules for a number followed by an identifier without white space in
    between. We don't want to parse the identifier as accidental alias. For
    instance, 123abc should be error, and we don't want it to be parsed as
    123 [AS] abc. */
invalid_token: /{decimal_digits}[A-Z_]/
invalid_token: /{hex_integer}[G-Z_]/
invalid_token: /{floating_point_literal}[A-Z_]/ {
    if (this.isAtDotAfterClosingBracketOrSoftKeyword()) {
      this.rewind(this._tokenOffset + 1);
      this.push(StateDotIdentifier);
      tok = token.TokenType.DOT;
    }
}

identifier_in_lexer: /{identifier}/ -1
invalid_token: /{unterminated_escaped_identifier}/
<dotIdentifier> identifier_in_lexer: /{generalized_identifier}/ {
    this.pop();
}
// Catch all rule for dotIdentifier state to return to normal state on
// encountering any unrecognizable character.
<dotIdentifier> catch_all: /[\x00-\xff]/    -2 (space) {
    this.rewind(this._tokenOffset);
    this.pop();
}
<*> whitespace_no_comments: /{whitespace_no_comments}/ (space)
<*> line_comment: /{line_comment}/ (space)
<*> block_comment: /{cs_comment}/ (space)
<*> invalid_token: /{cs_comment_begin}/

// begin punct

'+=': /+=/
'-=': /-=/
'|>': /\|>/
';': /;/
':': /:/
',': /,/
'.': /\./ {
    if (this.isAtDotAfterClosingBracketOrSoftKeyword()) {
      // When an identifier or unreserved keyword is followed by a dot, always
      // move to <dotIdentifier> lexer state. This can recognize keywords
      // as an identifier.
      this.push(StateDotIdentifier);
    }
}
'!=': /!=/
/* Don't recognize these in ARRAY<> or STRUCT<> context. */
'<>': /<>/ {
    if (this._previousToken == token.TokenType.ARRAY || this._previousToken == token.TokenType.STRUCT || this._previousToken == token.TokenType.RANGE) {
      // Match only the '<', and move to the same state that that production would
      // have moved to.
      this.push(StateInTemplatedType);
      this.rewind(this._tokenOffset + 1);
      tok = token.TokenType.LT;
    }
}
'->' (arrow): /->/
'=>': /=>/
'=': /=/
'>=': />=/
'<=': /<=/
'<<': /<</
<initial,inBetween> '>>': />>/
'<': /</ {
    if (this._previousToken == token.TokenType.ARRAY || this._previousToken == token.TokenType.STRUCT || this._previousToken == token.TokenType.RANGE) {
      // Switch to a mode that does not recognize >>. This only works as long as
      // there are no legal "independent" < and > inside array or struct types
      // (i.e., without ARRAY or STRUCT preceding) in the grammar. If there are,
      // then the state pushes and pops would become unbalanced, because ">" pops
      // this state.
      this.push(StateInTemplatedType);
    }
}
'>': />/ {
    if (this._state == StateInTemplatedType) {
      this.pop();
    }
}
// At opening bracket/parenthesis we need to suspend special modes such as
// IN_BETWEEN. This is popped again in the close rule below.
'{': /\{/ {
    this.push(StateInitial);
}
'[': /\[/ {
    // These need to suspend special modes such as IN_BETWEEN. This is popped
    // again in the close rule below.
    this.push(StateInitial);
}
'(': /\(/ {
    // These need to suspend special modes such as IN_BETWEEN. This is popped
    // again in the close rule below.
    this.push(StateInitial);
}
// At the closing bracket, switch to the parser state prior to
// the balancing opening bracket.
'}': /\}/ {
    this.popNonInitial(); // popping all InTemplatedType, inBetween and dotIdentifierState
    this.pop(); // pop the initial state introduced by the balancing bracket.
}
']': /\]/ {
    this.popNonInitial();
    this.pop();
}
')': /\)/ {
    this.popNonInitial();
    this.pop();
}
'+': /+/
'-': /-/
'*': /\*/
'/': /\//
'!': /!/
'~': /~/
'%': /%/
'@': /@/
'@@': /@@/
'|': /\|/
'||': /\|\|/
'&': /&/
'^': /\^/
'?': /\?/
'$': /$/

macro_argument_reference: /${decimal_digits}/

open_hint: /@{opt_whitespace}\{/ { this.rewind(this._tokenOffset + 1); }
open_integer_hint: /@{opt_whitespace}({decimal_digits}|{hex_integer})/ { this.rewind(this._tokenOffset + 1); }
backslash: /\\/

// begin nopattern

EDGE_ENDPOINT_PRECEDENCE:
PRIMARY_PRECEDENCE:
DOUBLE_AT_PRECEDENCE:
UNARY_PRECEDENCE:
UNARY_NOT_PRECEDENCE:

// begin invalidtoken

// In scanBytes mode, the lexer is consuming input byte by byte rather than
// rune by rune. This means that in all character classes we match bytes rather
// than runes (while outside of character classes we can match runes). The
// following regular expression matches exactly one non-ASCII utf8 rune (as
// a sequence of 2-4 bytes).
utf8rune_nonascii = /([\xc0-\xdf][\x80-\xbf]|[\xe0-\xef][\x80-\xbf]{2}|[\xf0-\xf7][\x80-\xbf]{3})/

// Invalid UTF-8 runes.
invalid_token: /{utf8rune_nonascii}/ -2

// end window of tokens generated from bison_parser.y

// Don't move this token: generation of the window of tokens requires a manual
// token after the window, right before the parser section.
// The `error` terminal enables the error recovery mechanism in the parser.
error:

:: parser

///////////////////////////////////////////////////////////////////////////////
// DO NOT CHANGE anything below: it gets auto-synced from the GoogleSQL grammar
// bison_parser.y START

// Number of shift-reduce conflicts are expected. Zero reduce-reduce conflicts expected.
%expect 27;

// Precedence declarations
%left KW_OR;
%left 'AND' ;
%nonassoc '=' '<>' '>' '<' '>=' '<=' '!=' 'LIKE' 'IN' 'DISTINCT' 'BETWEEN' 'IS' NOT_SPECIAL;
// Note: Textmapper does not support %precedence, only %nonassoc.
%nonassoc UNARY_NOT_PRECEDENCE;
%left '|';
%left '^';
%left '&';
%left '<<' '>>';
%left '+' '-';
%left '||';
%left '*' '/';
// Note: Textmapper does not support %precedence, only %nonassoc.
%nonassoc UNARY_PRECEDENCE;
// Note: Textmapper does not support %precedence, only %nonassoc.
%nonassoc DOUBLE_AT_PRECEDENCE;
%left PRIMARY_PRECEDENCE '(' '[' '.';

// For each of the rules below, there is a corresponding parse function. Example: sql.ParseFile(), sql.ParseScript(), etc.
%input sql_statement, script, next_statement, next_script_statement, next_statement_kind, expression, type;

%inject line_comment -> LineComment;
%inject block_comment -> BlockComment;
%inject invalid_token -> InvalidToken;
%inject macro_argument_reference -> KwMacroArgRef;

// Exclude keywords which are shifted as identifiers.
%generate exprStarters = set(first expression & ~keyword_as_identifier);

// Non-terminal declarations
opt_semicolon:
    ';'
  | %empty
;

sql_statement:
    unterminated_sql_statement opt_semicolon ;

next_script_statement:
    unterminated_statement ';'
  | unterminated_statement
;

next_statement:
    unterminated_sql_statement ';'
  | unterminated_sql_statement
;

%interface Stmt;

unterminated_statement -> Stmt:
    unterminated_sql_statement
  | unterminated_script_statement
  | invalid_statement
;

unterminated_sql_statement -> Stmt:
    sql_statement_body
  | hint sql_statement_body -> HintedStmt
;

unterminated_script_statement -> Stmt:
    if_statement
  | case_statement
  | begin_end_block
  | variable_declaration
  | while_statement
  | loop_statement
  | repeat_statement
  | for_in_statement
  | break_statement
  | continue_statement
  | return_statement
  | raise_statement
;


// startStatementSet is the set of keywords that can start a standard statement.
// terminated_statement would be a more exhaustive set of keywords that can
// start a segment but this would include the statement allowed in GoogleSQL
// scripts and include 'CASE', '(' which we ignore for now.
%generate startStatementSet = set(first sql_statement_body);

terminated_statement -> Stmt:
    unterminated_statement ';' ;

sql_statement_body -> Stmt:
    query_statement
  | alter_statement
  | analyze_statement
  | assert_statement
  | aux_load_data_statement
  | clone_data_statement
  | dml_statement
  | merge_statement
  | truncate_statement
  | begin_statement
  | set_statement
  | commit_statement
  | start_batch_statement
  | run_batch_statement
  | abort_batch_statement
  | create_constant_statement
  | create_database_statement
  | create_function_statement
  | create_procedure_statement
  | create_index_statement
  | create_privilege_restriction_statement
  | create_row_access_policy_statement
  | create_external_table_statement
  | create_external_table_function_statement
  | create_model_statement
  | create_schema_statement
  | create_snapshot_table_statement
  | create_table_function_statement
  | create_table_statement
  | create_view_statement
  | create_entity_statement
  | define_table_statement
  | describe_statement
  | execute_immediate
  | explain_statement
  | export_data_statement
  | export_model_statement
  | grant_statement
  | rename_statement
  | revoke_statement
  | rollback_statement
  | show_statement
  | drop_all_row_access_policies_statement
  | drop_statement
  | call_statement
  | import_statement
  | module_statement
;

query_statement -> QueryStmt:
    (query -> Query) ;

%interface AlterAction;

alter_action -> AlterAction:
    'SET' 'OPTIONS' syntax_problem? options_list                                                            -> SetOptionsAction
  | 'SET' 'AS' generic_entity_body                                                                          -> SetAsAction
  | 'ADD' table_constraint_spec                                                                             -> AddConstraintAction
  | 'ADD' primary_key_spec                                                                                  -> AddConstraintAction
  | 'ADD' 'CONSTRAINT' opt_if_not_exists identifier primary_key_or_table_constraint_spec                    -> AddConstraintAction
  | 'DROP' 'CONSTRAINT' opt_if_exists identifier                                                            -> DropConstraintAction
  | 'DROP' 'PRIMARY' 'KEY' opt_if_exists                                                                    -> DropPrimaryKeyAction
  | 'ALTER' 'CONSTRAINT' opt_if_exists identifier constraint_enforcement                                    -> AlterConstraintEnforcementAction
  | 'ALTER' 'CONSTRAINT' opt_if_exists identifier 'SET' 'OPTIONS' options_list                              -> AlterConstraintSetOptionsAction
  | 'ADD' 'COLUMN' opt_if_not_exists table_column_definition opt_column_position opt_fill_using_expression  -> AddColumnAction
  | 'DROP' 'COLUMN' opt_if_exists identifier                                                                -> DropColumnAction
  | 'RENAME' 'COLUMN' opt_if_exists identifier 'TO' identifier                                              -> RenameColumnAction
  | 'ALTER' 'COLUMN' opt_if_exists identifier 'SET' 'DATA' 'TYPE' field_schema                              -> AlterColumnTypeAction
  | 'ALTER' 'COLUMN' opt_if_exists identifier 'SET' 'OPTIONS' options_list                                  -> AlterColumnOptionsAction
  | 'ALTER' 'COLUMN' opt_if_exists identifier 'SET' 'DEFAULT' expression                                    -> AlterColumnSetDefaultAction
  | 'ALTER' 'COLUMN' opt_if_exists identifier 'DROP' 'DEFAULT'                                              -> AlterColumnDropDefaultAction
  | 'ALTER' 'COLUMN' opt_if_exists identifier 'DROP' 'NOT' 'NULL'                                           -> AlterColumnDropNotNullAction
  | 'RENAME' 'TO' path_expression                                                                           -> RenameToClause
  | 'SET' 'DEFAULT' collate_clause                                                                          -> SetCollateClause
  | 'ADD' 'ROW' 'DELETION' 'POLICY' opt_if_not_exists '(' expression ')'                                    -> AddTTLAction
  | 'REPLACE' 'ROW' 'DELETION' 'POLICY' opt_if_exists '(' expression ')'                                    -> ReplaceTTLAction
  | 'DROP' 'ROW' 'DELETION' 'POLICY' opt_if_exists                                                          -> DropTTLAction
  | 'ALTER' generic_sub_entity_type opt_if_exists identifier alter_action                                   -> AlterSubEntityAction
  | 'ADD' generic_sub_entity_type opt_if_not_exists identifier opt_options_list                             -> AddSubEntityAction
  | 'DROP' generic_sub_entity_type opt_if_exists identifier                                                 -> DropSubEntityAction
  | spanner_alter_column_action
  | spanner_set_on_delete_action
;

alter_action_list:
    alter_action
  | alter_action_list ',' alter_action
;

privilege_restriction_alter_action:
    restrict_to_clause
  | 'ADD' opt_if_not_exists possibly_empty_grantee_list
  | 'REMOVE' opt_if_exists possibly_empty_grantee_list
;

privilege_restriction_alter_action_list:
    privilege_restriction_alter_action
  | privilege_restriction_alter_action_list ',' privilege_restriction_alter_action
;

row_access_policy_alter_action -> RowAccessPolicyAlterAction:
    grant_to_clause
  | 'FILTER' 'USING' '(' expression ')'
  | 'REVOKE' 'FROM' '(' grantee_list ')'
  | 'REVOKE' 'FROM' 'ALL'
  | 'RENAME' 'TO' identifier
;

row_access_policy_alter_action_list:
    row_access_policy_alter_action
  | row_access_policy_alter_action_list ',' row_access_policy_alter_action
;

schema_object_kind:
    'AGGREGATE' 'FUNCTION'
  | 'CONSTANT'
  | 'DATABASE'
  | 'EXTERNAL' table_or_table_function
  | 'FUNCTION'
  | 'INDEX'
  | 'MATERIALIZED' 'VIEW'
  | 'MODEL'
  | 'PROCEDURE'
  | 'SCHEMA'
  | 'VIEW'
;

alter_statement -> AlterStmt:
    'ALTER' table_or_table_function opt_if_exists (maybe_dashed_path_expression -> TableName/RetainText) syntax_problem? alter_action_list
  | 'ALTER' schema_object_kind opt_if_exists path_expression alter_action_list
  | 'ALTER' generic_entity_type opt_if_exists path_expression alter_action_list
  | 'ALTER' generic_entity_type opt_if_exists alter_action_list
  | 'ALTER' 'PRIVILEGE' 'RESTRICTION' opt_if_exists 'ON' privilege_list 'ON' identifier path_expression privilege_restriction_alter_action_list
  | 'ALTER' 'ROW' 'ACCESS' 'POLICY' opt_if_exists identifier 'ON' path_expression row_access_policy_alter_action_list
  | 'ALTER' 'ALL' 'ROW' 'ACCESS' 'POLICIES' 'ON' path_expression row_access_policy_alter_action
;

opt_input_output_clause:
    'INPUT' table_element_list 'OUTPUT' table_element_list -> InputOutputClause
  | %empty
;

opt_transform_clause:
    'TRANSFORM' '(' select_list ')'
  | %empty
;

assert_statement -> AssertStmt:
    'ASSERT' expression opt_description ;

opt_description:
    'AS' string_literal
  | %empty
;

analyze_statement -> AnalyzeStmt:
    'ANALYZE' opt_options_list opt_table_and_column_info_list ;

opt_table_and_column_info_list:
    table_and_column_info_list
  | %empty
;

table_and_column_info_list:
    table_and_column_info
  | table_and_column_info_list ',' table_and_column_info
;

table_and_column_info:
    maybe_dashed_path_expression opt_column_list ;

// TODO(b/263109796): add a `isolation_level`: Following are the standard values for isolation_level:
// - READ UNCOMMITTED
// - READ COMMITTED
// - REPEATABLE READ
// - SERIALIZABLE
// https://g3doc.corp.google.com/company/teams/googlesql/reference/transactions.md?cl=head
// There is no need for an identifier here.
transaction_mode:
    'READ' 'ONLY'
  | 'READ' 'WRITE'
  | 'ISOLATION' 'LEVEL' identifier
  | 'ISOLATION' 'LEVEL' identifier identifier
;

transaction_mode_list:
    transaction_mode
  | transaction_mode_list ',' transaction_mode
;

opt_transaction_mode_list:
    transaction_mode_list
  | %empty
;

begin_statement -> BeginStmt:
    begin_transaction_keywords opt_transaction_mode_list ;

begin_transaction_keywords:
    'START' transaction_keyword
  | 'BEGIN' opt_transaction_keyword
;

transaction_keyword:
    'TRANSACTION' ;

opt_transaction_keyword:
    transaction_keyword
  | %empty
;

// Divergence between bison_parser.y and sql.tm.
// The generated nonterm listed below must be deleted:
// - set_statement
// The 1 nonterm after this comment is manually crafted, and should replace the
// generated nonterm.
//
// rationale: the reference grammar set `identifier` instead of `path_expression`
// in the rule below:
//    | 'SET' path_expression '=' expression
// This breaks common option such as:
//    SET ModifyRequest_.keep_going=true
// TODO(b/265668444): modify the Bison rule to use path_expression.
set_statement -> SetStmt:
    'SET' 'TRANSACTION' transaction_mode_list
  | 'SET' (path_expression -> PathExpr) '=' expression
  | 'SET' named_parameter_expression '=' expression
  | 'SET' system_variable_expression '=' expression
  | 'SET' '(' (identifier_list -> IDList) ')' '=' expression
  | 'SET' '(' ')'                             -> SyntaxProblem
  | 'SET' identifier ',' identifier_list '='  -> SyntaxProblem
;

commit_statement -> CommitStmt:
    'COMMIT' opt_transaction_keyword ;

rollback_statement -> RollbackStmt:
    'ROLLBACK' opt_transaction_keyword ;

start_batch_statement -> StartBatchStmt:
    'START' 'BATCH' opt_identifier ;

run_batch_statement -> RunBatchStmt:
    'RUN' 'BATCH' ;

abort_batch_statement -> AbortBatchStmt:
    'ABORT' 'BATCH' ;

create_constant_statement -> CreateConstStmt:
    'CREATE' opt_or_replace opt_create_scope 'CONSTANT' opt_if_not_exists (path_expression -> ConstName) '=' expression ;

create_database_statement -> CreateDatabaseStmt:
    'CREATE' 'DATABASE' (path_expression -> DatabaseName) opt_options_list ;

unordered_options_body:
    as_sql_function_body_or_string opt_options_list
  | 'OPTIONS' options_list opt_as_sql_function_body_or_string
  | %empty
;

create_function_statement -> CreateFuncStmt:
    'CREATE' opt_or_replace opt_create_scope opt_aggregate
    'FUNCTION' opt_if_not_exists function_declaration
    opt_function_returns opt_sql_security_clause opt_determinism_level
    opt_language_or_remote_with_connection unordered_options_body ;

opt_aggregate:
    'AGGREGATE' -> Modifier/RetainText
  | %empty
;

opt_not_aggregate:
    'NOT' 'AGGREGATE' -> Modifier/RetainText
  | %empty
;

function_declaration:
    ((path_expression -> FuncName) function_parameters) -> FuncDecl ;

function_parameter -> FuncParam:
    identifier type_or_tvf_schema opt_as_alias_with_required_as opt_default_expression opt_not_aggregate
  | type_or_tvf_schema opt_as_alias_with_required_as opt_not_aggregate
;

function_parameters_list:
    function_parameter
  | function_parameters_list ',' function_parameter
  | function_parameters_list (',' -> SyntaxProblem)
;

function_parameters:
    '(' (function_parameters_list -> FuncParams) ')'
  | '(' ')'
;

unlabeled_begin_end_block_or_language_as_code:
    unlabeled_begin_end_block
  | 'LANGUAGE' identifier opt_as_code
;

create_procedure_statement -> CreateProcedureStmt:
    'CREATE' opt_or_replace opt_create_scope 'PROCEDURE' opt_if_not_exists (path_expression -> ProcedureName) procedure_parameters opt_with_connection_clause opt_options_list unlabeled_begin_end_block_or_language_as_code ;

procedure_parameters_prefix:
    '(' procedure_parameter
  | procedure_parameters_prefix ',' procedure_parameter
;

procedure_parameters:
    procedure_parameters_prefix ')'
  | '(' ')'
;

procedure_parameter_termination:
    ')'
  | ','
;

procedure_parameter -> FuncParam:
    opt_procedure_parameter_mode identifier type_or_tvf_schema
  | opt_procedure_parameter_mode identifier procedure_parameter_termination -> SyntaxProblem
;

opt_procedure_parameter_mode:
    'IN'
  | 'OUT'
  | 'INOUT'
  | %empty
;

opt_returns:
    'RETURNS' type_or_tvf_schema
  | %empty
;

opt_function_returns:
    opt_returns ;

opt_determinism_level:
    'DETERMINISTIC'
  | 'NOT' 'DETERMINISTIC'
  | 'IMMUTABLE'
  | 'STABLE'
  | 'VOLATILE'
  | %empty
;

opt_language:
    'LANGUAGE' identifier
  | %empty
;

remote_with_connection_clause:
    'REMOTE' opt_with_connection_clause
  | with_connection_clause
;

opt_remote_with_connection_clause:
    remote_with_connection_clause
  | %empty
;

opt_language_or_remote_with_connection:
    'LANGUAGE' identifier opt_remote_with_connection_clause
  | remote_with_connection_clause opt_language
  | %empty
;

opt_sql_security_clause:
    'SQL' 'SECURITY' sql_security_clause_kind
  | %empty
;

sql_security_clause_kind:
    'INVOKER'
  | 'DEFINER'
;

as_sql_function_body_or_string:
    'AS' (sql_function_body -> FuncBody)
  | 'AS' string_literal
;

opt_as_sql_function_body_or_string:
    as_sql_function_body_or_string
  | %empty
;

opt_as_code:
    'AS' string_literal
  | %empty
;

path_expression_or_string:
    path_expression
  | string_literal -> ID/RetainText
;

path_expression_or_default:
    path_expression
  | 'DEFAULT' -> DefaultLiteral
;

sql_function_body:
    '(' (expression | syntax_problem) ')'
  | '(' 'SELECT'
;

restrict_to_clause:
    'RESTRICT' 'TO' possibly_empty_grantee_list ;

opt_restrict_to_clause:
    restrict_to_clause
  | %empty
;

grant_to_clause:
    'GRANT' 'TO' '(' grantee_list ')' ;

create_row_access_policy_grant_to_clause:
    grant_to_clause
  | 'TO' grantee_list
;

opt_create_row_access_policy_grant_to_clause:
    create_row_access_policy_grant_to_clause
  | %empty
;

opt_filter:
    'FILTER'
  | %empty
;

filter_using_clause:
    opt_filter 'USING' '(' expression ')' ;

create_privilege_restriction_statement -> CreatePrivilegeRestrictionStmt:
    'CREATE' opt_or_replace 'PRIVILEGE' 'RESTRICTION' opt_if_not_exists 'ON' privilege_list 'ON' identifier path_expression opt_restrict_to_clause ;

create_row_access_policy_statement -> CreateRowAccessPolicyStmt:
    'CREATE' opt_or_replace 'ROW' opt_access 'POLICY' opt_if_not_exists opt_identifier 'ON' path_expression opt_create_row_access_policy_grant_to_clause filter_using_clause ;

with_partition_columns_clause:
    'WITH' 'PARTITION' 'COLUMNS' opt_table_element_list ;

with_connection_clause:
    'WITH' connection_clause ;

opt_external_table_with_clauses:
    with_partition_columns_clause with_connection_clause
  | with_partition_columns_clause
  | with_connection_clause
  | %empty
;

create_external_table_statement -> CreateExternalTableStmt:
    'CREATE' opt_or_replace opt_create_scope 'EXTERNAL' 'TABLE' opt_if_not_exists (maybe_dashed_path_expression -> ExternalTableName/RetainText) opt_table_element_list opt_like_path_expression opt_default_collate_clause opt_external_table_with_clauses opt_options_list ;

create_external_table_function_statement -> CreateExternalTableFuncStmt:
    'CREATE' opt_or_replace opt_create_scope 'EXTERNAL' 'TABLE' 'FUNCTION' ;

create_index_statement -> CreateIndexStmt:
    'CREATE' opt_or_replace opt_unique opt_spanner_null_filtered opt_search 'INDEX' opt_if_not_exists (path_expression -> IndexName) 'ON' path_expression opt_as_alias opt_index_unnest_expression_list index_order_by opt_index_storing_list opt_options_list opt_spanner_index_interleave_clause ;

create_schema_statement -> CreateSchemaStmt:
    'CREATE' opt_or_replace 'SCHEMA' opt_if_not_exists path_expression opt_default_collate_clause opt_options_list ;

create_snapshot_table_statement -> CreateSnapshotTableStmt:
    'CREATE' opt_or_replace 'SNAPSHOT' 'TABLE' opt_if_not_exists maybe_dashed_path_expression 'CLONE' clone_data_source opt_options_list ;

create_table_function_statement -> CreateTableFuncStmt:
    'CREATE' opt_or_replace opt_create_scope 'TABLE' 'FUNCTION'
    opt_if_not_exists tvf_declaration opt_returns opt_sql_security_clause
    opt_options_list opt_language opt_as_query_or_string ;

create_table_statement -> CreateTableStmt:
    'CREATE' opt_or_replace opt_create_scope 'TABLE' opt_if_not_exists (maybe_dashed_path_expression -> TableName/RetainText) opt_table_element_list opt_spanner_table_options opt_like_path_expression opt_clone_table opt_copy_table opt_default_collate_clause opt_partition_by_clause_no_hint opt_cluster_by_clause_no_hint opt_ttl_clause opt_options_list opt_as_query ;

tvf_declaration:
    ((path_expression -> TVFName) function_parameters) -> FuncDecl ;

append_or_overwrite:
    'INTO'
  | 'OVERWRITE'
;

aux_load_data_from_files_options_list:
    'FROM' 'FILES' options_list ;

aux_load_data_statement -> AuxLoadStmt:
    'LOAD' 'DATA' append_or_overwrite maybe_dashed_path_expression opt_table_element_list opt_collate_clause opt_partition_by_clause_no_hint opt_cluster_by_clause_no_hint opt_options_list aux_load_data_from_files_options_list opt_external_table_with_clauses ;

generic_entity_type:
    identifier_in_lexer ;

generic_sub_entity_type:
    identifier_in_lexer
  | 'REPLICA'
;

generic_entity_body:
    json_literal
  | string_literal
;

opt_generic_entity_body:
    'AS' generic_entity_body
  | %empty
;

create_entity_statement -> CreateEntityStmt:
    'CREATE' opt_or_replace generic_entity_type opt_if_not_exists path_expression opt_options_list opt_generic_entity_body ;

create_model_statement -> CreateModelStmt:
    'CREATE' opt_or_replace opt_create_scope 'MODEL' opt_if_not_exists (path_expression -> ModelName) opt_input_output_clause opt_transform_clause opt_remote_with_connection_clause opt_options_list opt_as_query_or_aliased_query_list ;

opt_table_element_list:
    table_element_list
  | %empty
;

table_element_list:
    table_element_list_prefix ')'
  | '(' ')'
;

table_element_list_prefix:
    '(' table_element
  | table_element_list_prefix ',' table_element
  | table_element_list_prefix ','
;

// TODO(b/263109796): Make TableElement an interface.
//
// The original BISON grammar models a statement such as:
//
//   CREATE TABLE t1 (a int32, PRIMARY KEY(a))
//
// as:
//
//   CreateTableStatement
//     PathExpression      `t1`
//     TableElementList
//       ColumnDefinition  `a int32`
//       PrimaryKey        `PRIMARY KEY(a)`
//
// While we'll currently map it as:
//
//   CreateTableStmt
//     TableName           `t1`
//     TableElement        `a int32`
//       ColumnDefinition
//     TableElement        `PRIMARY KEY(a)`
//       // Note the missing child node
//
// The solution here is to make TableElement an interface and ensure that both
// `table_column_definition` and `table_constraint_definition` return appropriate
// AST nodes.
//
// I don't think we need to have TableElementList like in the original BISON
// grammar.
table_element -> TableElement:
    table_column_definition
  | table_constraint_definition
;

table_column_definition -> ColumnDefinition:
    identifier table_column_schema opt_column_attributes opt_options_list ;

table_column_schema:
    column_schema_inner opt_collate_clause opt_column_info
  | generated_column_info
;

%interface ColumnSchemaType;

simple_column_schema_inner -> SimpleColumnSchema as ColumnSchemaType:
    path_expression
  | 'INTERVAL'
;

array_column_schema_inner -> ArrayColumnSchema as ColumnSchemaType:
    'ARRAY' '<' field_schema '>' ;

struct_column_field -> StructColumnField:
    column_schema_inner opt_collate_clause opt_field_attributes
  | identifier field_schema
;

struct_column_schema_prefix:
    'STRUCT' '<' struct_column_field
  | struct_column_schema_prefix ',' struct_column_field
;

struct_column_schema_inner -> StructColumnSchema as ColumnSchemaType:
    'STRUCT' '<' '>'
  | struct_column_schema_prefix '>'
;

raw_column_schema_inner -> ColumnSchemaType:
    simple_column_schema_inner
  | array_column_schema_inner
  | struct_column_schema_inner
;

column_schema_inner:
    raw_column_schema_inner opt_type_parameters ;

generated_as_keywords:
    'GENERATED' 'AS'
  | 'AS'
;

stored_mode:
    'STORED' 'VOLATILE'
  | 'STORED'
  | %empty
;

generated_column_info -> GeneratedColumnInfo:
    generated_as_keywords '(' expression ')' stored_mode ;

invalid_generated_column:
    generated_column_info
  | %empty
;

default_column_info:
    'DEFAULT' expression ;

invalid_default_column:
    default_column_info
  | %empty
;

opt_column_info:
    generated_column_info invalid_default_column
  | default_column_info invalid_generated_column
  | %empty
;

field_schema:
    column_schema_inner opt_collate_clause opt_field_attributes opt_options_list ;

primary_key_column_attribute -> PrimaryKeyColumnAttribute:
    'PRIMARY' 'KEY' ;

foreign_key_column_attribute -> ForeignKeyColumnAttribute:
    opt_constraint_identity foreign_key_reference ;

hidden_column_attribute -> HiddenColumnAttribute:
    'HIDDEN' ;

not_null_column_attribute -> NotNullColumnAttribute:
    'NOT' 'NULL' ;

%interface ColumnAttribute;

column_attribute -> ColumnAttribute:
    primary_key_column_attribute
  | foreign_key_column_attribute
  | hidden_column_attribute
  | not_null_column_attribute
;

// TODO(b/263109796): fix the enforcement constraint on the foreign and primary key
// google3/storage/googlesql/parser/bison_parser.y;rcl=488990182;l=3557
// "Conceptually, a foreign key column reference is defined by this rule:" ...
column_attributes:
    column_attribute
  | column_attributes column_attribute
  | column_attributes constraint_enforcement
;

opt_column_attributes:
    column_attributes
  | %empty
;

opt_field_attributes:
    not_null_column_attribute
  | %empty
;

column_position:
    'PRECEDING' identifier
  | 'FOLLOWING' identifier
;

opt_column_position:
    column_position
  | %empty
;

fill_using_expression:
    'FILL' 'USING' expression ;

opt_fill_using_expression:
    fill_using_expression
  | %empty
;

// TODO(b/263109796): Use TableElement here as well once we make it an interface.
table_constraint_spec:
    'CHECK' '(' expression ')' opt_constraint_enforcement opt_options_list                         -> CheckConstraint
  | 'FOREIGN' 'KEY' column_list foreign_key_reference opt_constraint_enforcement opt_options_list  -> ForeignKey
;

primary_key_element -> PrimaryKeyElement:
    identifier opt_asc_or_desc opt_null_order ;

primary_key_element_list_prefix:
    '(' primary_key_element
  | primary_key_element_list_prefix ',' primary_key_element
;

primary_key_element_list:
    primary_key_element_list_prefix ')'
  | '(' ')'
;

primary_key_spec -> PrimaryKey:
    'PRIMARY' 'KEY' primary_key_element_list opt_constraint_enforcement opt_options_list ;

primary_key_or_table_constraint_spec:
    primary_key_spec
  | table_constraint_spec
;

table_constraint_definition:
    primary_key_spec
  | table_constraint_spec
  | identifier identifier table_constraint_spec
;

foreign_key_reference:
    'REFERENCES' path_expression column_list opt_foreign_key_match opt_foreign_key_actions ;

opt_foreign_key_match:
    'MATCH' foreign_key_match_mode
  | %empty
;

foreign_key_match_mode:
    'SIMPLE'
  | 'FULL'
  | NOT_SPECIAL 'DISTINCT'
;

opt_foreign_key_actions:
    foreign_key_on_update opt_foreign_key_on_delete
  | foreign_key_on_delete opt_foreign_key_on_update
  | %empty
;

opt_foreign_key_on_update:
    foreign_key_on_update
  | %empty
;

opt_foreign_key_on_delete:
    foreign_key_on_delete
  | %empty
;

foreign_key_on_update:
    'ON' 'UPDATE' foreign_key_action ;

foreign_key_on_delete:
    'ON' 'DELETE' foreign_key_action ;

foreign_key_action:
    'NO' 'ACTION'
  | 'RESTRICT'
  | 'CASCADE'
  | 'SET' 'NULL'
;

opt_constraint_identity:
    'CONSTRAINT' identifier
  | %empty
;

opt_constraint_enforcement:
    constraint_enforcement
  | %empty
;

constraint_enforcement:
    'ENFORCED'
  | 'NOT' 'ENFORCED'
;

table_or_table_function:
    'TABLE' 'FUNCTION'
  | 'TABLE'
;

tvf_schema_column -> TVFSchemaColumn:
    identifier type
  | type
;

tvf_schema_prefix:
    'TABLE' '<' tvf_schema_column
  | tvf_schema_prefix ',' tvf_schema_column
;

tvf_schema -> TVFSchema:
    tvf_schema_prefix '>' ;

opt_recursive:
    'RECURSIVE'
  | %empty
;

create_view_statement -> CreateViewStmt:
    'CREATE' opt_or_replace opt_create_scope opt_recursive 'VIEW' opt_if_not_exists (maybe_dashed_path_expression -> ViewName) opt_column_with_options_list opt_sql_security_clause opt_options_list as_query
  | 'CREATE' opt_or_replace 'MATERIALIZED' opt_recursive 'VIEW' opt_if_not_exists (maybe_dashed_path_expression -> ViewName) opt_column_with_options_list opt_sql_security_clause opt_partition_by_clause_no_hint opt_cluster_by_clause_no_hint opt_options_list as_query
;

as_query:
    'AS' (query -> Query) ;

opt_as_query:
    as_query
  | %empty
;

opt_as_query_or_string:
    as_query
  | 'AS' string_literal
  | %empty
;

opt_as_query_or_aliased_query_list:
    as_query
  | 'AS' '(' aliased_query_list ')'
  | %empty
;

opt_if_not_exists:
    'IF' 'NOT' 'EXISTS'
  | %empty
;

describe_statement -> DescribeStmt:
    describe_keyword describe_info ;

describe_info:
    identifier maybe_slashed_or_dashed_path_expression opt_from_path_expression
  | maybe_slashed_or_dashed_path_expression opt_from_path_expression
;

opt_from_path_expression:
    'FROM' maybe_slashed_or_dashed_path_expression
  | %empty
;

explain_statement -> ExplainStmt:
    'EXPLAIN' unterminated_sql_statement ;

export_data_statement -> ExportDataStmt:
    'EXPORT' 'DATA' opt_with_connection_clause opt_options_list 'AS' (query -> Query) ;

export_model_statement -> ExportModelStmt:
    'EXPORT' 'MODEL' path_expression opt_with_connection_clause opt_options_list ;

grant_statement -> GrantStmt:
    'GRANT' privileges 'ON' identifier path_expression 'TO' grantee_list
  | 'GRANT' privileges 'ON' path_expression 'TO' grantee_list
;

revoke_statement -> RevokeStmt:
    'REVOKE' privileges 'ON' identifier path_expression 'FROM' grantee_list
  | 'REVOKE' privileges 'ON' path_expression 'FROM' grantee_list
;

privileges:
    'ALL' opt_privileges_keyword
  | privilege_list
;

opt_privileges_keyword:
    'PRIVILEGES'
  | %empty
;

privilege_list:
    privilege
  | privilege_list ',' privilege
;

privilege:
    privilege_name opt_path_expression_list_with_parens ;

privilege_name:
    identifier
  | 'SELECT'
;

rename_statement -> RenameStmt:
    'RENAME' identifier path_expression 'TO' path_expression ;

import_statement -> ImportStmt:
    'IMPORT' import_type (path_expression_or_string -> ImportPath) opt_as_or_into_alias opt_options_list ;

module_statement -> ModuleStmt:
    'MODULE' (path_expression -> ModuleName) opt_options_list ;

index_order_by_prefix:
    '(' ordering_expression
  | index_order_by_prefix ',' ordering_expression
;

index_all_columns:
    '(' 'ALL' 'COLUMNS' ')' ;

index_order_by:
    index_order_by_prefix ')'
  | index_all_columns
;

index_unnest_expression_list:
    unnest_expression_with_opt_alias_and_offset
  | index_unnest_expression_list unnest_expression_with_opt_alias_and_offset
;

opt_index_unnest_expression_list:
    index_unnest_expression_list
  | %empty
;

index_storing_expression_list_prefix:
    '(' expression
  | index_storing_expression_list_prefix ',' expression
;

index_storing_expression_list:
    index_storing_expression_list_prefix ')' ;

index_storing_list:
    'STORING' index_storing_expression_list ;

opt_index_storing_list:
    index_storing_list
  | %empty
;

column_list_prefix:
    '(' identifier
  | column_list_prefix ',' identifier
;

column_list:
    column_list_prefix ')' ;

opt_column_list:
    column_list
  | %empty
;

column_with_options -> ColumnWithOptions:
    identifier opt_options_list ;

column_with_options_list_prefix:
    '(' column_with_options
  | column_with_options_list_prefix ',' column_with_options
;

column_with_options_list:
    column_with_options_list_prefix ')' ;

opt_column_with_options_list:
    column_with_options_list
  | %empty
;

grantee_list:
    string_literal_or_parameter
  | grantee_list ',' string_literal_or_parameter
;

grantee_list_with_parens_prefix:
    '(' string_literal_or_parameter
  | grantee_list_with_parens_prefix ',' string_literal_or_parameter
;

possibly_empty_grantee_list:
    grantee_list_with_parens_prefix ')'
  | '(' ')'
;

show_statement -> ShowStmt:
    'SHOW' show_target opt_from_path_expression opt_like_string_literal ;

show_target:
    'MATERIALIZED' 'VIEWS'
  | 'TABLES'
  | identifier
;

opt_like_string_literal:
    'LIKE' string_literal
  | %empty
;

opt_like_path_expression:
    'LIKE' maybe_dashed_path_expression
  | %empty
;

opt_clone_table:
    'CLONE' clone_data_source
  | %empty
;

opt_copy_table:
    'COPY' copy_data_source
  | %empty
;

all_or_distinct -> SetOperationAllOrDistinct:
    'ALL'
  | 'DISTINCT'
;

// TODO(b/263109796): Correctly tokenize EXCEPT within set operations as EXCEPT_IN_SET_OP
query_set_operation_type -> SetOperationType:
    'UNION'
  | EXCEPT_IN_SET_OP
  | 'INTERSECT'
;

query_primary_or_set_operation:
    query_primary
  | query_set_operation
;

query_primary_or_set_operation_maybe_expression:
    query_primary_maybe_expression
  | query_set_operation_maybe_expression
;

parenthesized_query:
    '(' (query -> Query) ')' ;


select_or_from_keyword:
    'SELECT'
  | 'FROM'
;

bad_keyword_after_from_query:
    'WHERE'
  | 'SELECT'
  | 'GROUP'
;

bad_keyword_after_from_query_allows_parens:
    'ORDER'
  | 'UNION'
  | 'INTERSECT'
  | EXCEPT_IN_SET_OP
  | 'LIMIT'
;

query_without_pipe_operators:
    ((with_clause | 'WITH' syntax_problem) -> WithClause) query_primary_or_set_operation opt_order_by_clause opt_limit_offset_clause
  | with_clause_with_trailing_comma (select_or_from_keyword -> SyntaxProblem)
  | query_primary_or_set_operation opt_order_by_clause opt_limit_offset_clause
  | opt_with_clause (from_clause -> FromQuery)
  | opt_with_clause (from_clause bad_keyword_after_from_query -> SyntaxProblem)
  | opt_with_clause (from_clause bad_keyword_after_from_query_allows_parens -> SyntaxProblem)
;

query:
    query_without_pipe_operators
  | query pipe_and_pipe_operator
;


subpipeline_prefix:
    '('
  | subpipeline_prefix pipe_and_pipe_operator

;

subpipeline -> SubPipeline:
    subpipeline_prefix ')' ;

opt_subpipeline:
    subpipeline
  | %empty
;

pipe_and_pipe_operator:
    '|>' pipe_operator ;

%interface PipeClause;

pipe_operator -> PipeClause:
    pipe_where
  | pipe_select
  | pipe_extend
  | pipe_rename
  | pipe_aggregate
  | pipe_limit_offset
  | pipe_set_operation
  | pipe_order_by
  | pipe_join
  | pipe_call
  | pipe_window
  | pipe_distinct
  | pipe_tablesample
  | pipe_as
  | pipe_static_describe
  | pipe_assert
  | pipe_log
  | pipe_drop
  | pipe_set
  | pipe_pivot
  | pipe_unpivot
;

pipe_call -> PipeCall:
    'CALL' tvf ; // opt_as_alias TODO(b/302229800): fix this divergence.

pipe_window -> PipeWindow:
    'WINDOW' pipe_extend_item_list ;

pipe_distinct -> PipeDistinct:
    'DISTINCT' ;

pipe_tablesample -> PipeTablesample:
    sample_clause ;

pipe_as -> PipeAs:
    'AS' (identifier -> Alias) ;

pipe_static_describe -> PipeStaticDescribe:
    'STATIC_DESCRIBE' ;

// In the Bison grammar, 'ASSERT' is outside the span of the PipeAssert
// the Bison rule should be modified so that the 'ASSERT' goes into the
// pipe_assert nonterminal.
// TODO(b/265668444): modify the Bison rule to use path_expression.
pipe_assert_base:
    'ASSERT' expression
  | pipe_assert_base ',' expression
;

pipe_assert -> PipeAssert:
    pipe_assert_base opt_comma ;

pipe_log -> PipeLog:
    'LOG' opt_hint opt_subpipeline ;

pipe_drop -> PipeDrop:
    'DROP' (identifier_list -> IDList) opt_comma ;


pipe_set_item -> PipeSetItem:
    path_expression '=' expression ;

pipe_set_item_list:
    pipe_set_item
  | pipe_set_item_list ',' pipe_set_item
;

pipe_set:
    'SET' (pipe_set_item_list -> PipeSet) opt_comma ;

pipe_pivot -> PipePivot:
    pivot_clause opt_as_alias ;

pipe_unpivot -> PipeUnpivot:
    unpivot_clause opt_as_alias ;


pipe_join -> PipeJoin:
    opt_natural join_type join_hint 'JOIN' opt_hint table_primary opt_on_or_using_clause ;

pipe_order_by -> PipeOrderBy:
    order_by_clause_with_opt_comma ;

opt_corresponding_outer_mode:
    FULL_IN_SET_OP opt_outer  -> SetOperationColumnPropagationMode
  | 'OUTER'                   -> SetOperationColumnPropagationMode
  | LEFT_IN_SET_OP opt_outer  -> SetOperationColumnPropagationMode
  | %empty
;

opt_strict:
    'STRICT' -> SetOperationColumnPropagationMode
  | %empty
;

opt_column_match_suffix:
    'CORRESPONDING'                   -> SetOperationColumnMatchMode
  | 'CORRESPONDING' 'BY' column_list  -> SetOperationColumnMatchMode
  | %empty
;


pipe_selection_item_with_order:
    expression opt_grouping_item_order                             -> SelectColumn
  | expression (identifier -> Alias) opt_grouping_item_order       -> SelectColumn
  | expression ('AS' identifier -> Alias) opt_grouping_item_order  -> SelectColumn
;

pipe_selection_item_list_no_comma_with_order:
    pipe_selection_item_with_order
  | pipe_selection_item_list_no_comma_with_order ','
      pipe_selection_item_with_order
;

pipe_selection_item_list_with_order:
    pipe_selection_item_list_no_comma_with_order opt_comma ;

pipe_selection_item_list_with_order_or_empty:
    pipe_selection_item_list_with_order -> SelectList
  | %empty
;

pipe_rename_item -> PipeRenameItem:
    (identifier -> PipeRenameOldName) opt_as (identifier -> PipeRenameNewName)
  | identifier '.' -> SyntaxProblem
;

pipe_rename_item_list:
    pipe_rename_item
  | pipe_rename_item_list ',' pipe_rename_item
;

pipe_rename -> PipeRename:
    'RENAME' pipe_rename_item_list opt_comma ;


opt_comma:
    ','
  | %empty
;

pipe_aggregate -> PipeAggregate:
    'AGGREGATE' (pipe_selection_item_list_with_order_or_empty opt_group_by_clause_with_opt_comma<+andorder> -> Select) ;


pipe_set_operation -> PipeSetOperation:
    set_operation_metadata parenthesized_query
  | pipe_set_operation ',' parenthesized_query
;

pipe_where -> PipeWhere:
    where_clause ;

pipe_select -> PipeSelect:
    select_clause ;

pipe_limit_offset -> PipeLimitOffset:
    limit_offset_clause ;


pipe_extend -> PipeExtend:
    'EXTEND' pipe_extend_item_list ;

pipe_extend_item -> SelectColumn:
    expression
  | expression (identifier -> Alias)
  | expression ('AS' identifier -> Alias)
;

pipe_extend_item_list_no_comma:
    pipe_extend_item
  | pipe_extend_item_list_no_comma ',' pipe_extend_item
;

pipe_extend_item_list -> SelectList:
    pipe_extend_item_list_no_comma opt_comma ;

select_clause:
    'SELECT' opt_hint opt_select_with opt_all_or_distinct opt_select_as_clause select_list
  | 'SELECT' opt_hint opt_select_with opt_all_or_distinct opt_select_as_clause ('FROM' -> SyntaxProblem)
;

query_maybe_expression -> QueryExpr:
    with_clause query_primary_or_set_operation_maybe_expression opt_order_by_clause opt_limit_offset_clause -> Query
  //| with_clause_with_trailing_comma ('SELECT' -> SyntaxProblem)                                              -> Query
  | query_primary_or_set_operation_maybe_expression opt_order_by_clause opt_limit_offset_clause -> Query
;

// While query set operations such as UNION ALL and INTERSECT DISTINCT look like
// binary operators, they aren't actually parsed as a left-associative syntax
// tree in the BISON grammar as you'd expect. Instead, a query such as:
//
//   SELECT 1
//   UNION ALL
//   SELECT 2
//   INTERSECT ALL
//   SELECT 3;
//
// Gets parsed as:
//
//   SetOperation(UNION ALL)
//     SetOperationMetadataList
//       SetOperationMetadata(UNION ALL)  <- metadata for the first set operation
//         SetOperationType(UNION)
//         SetOperationAllOrDistinct(ALL)
//       SetOperationMetadata(INTERSECT ALL) <- metadata for the second set operation
//         SetOperationType(INTERSECT)
//         SetOperationAllOrDistinct(ALL)
//     Select
//     Select
//     Select
//
// The GoogleSQL BISON parser runs checks in the generated C++ code to reject
// operators that are incompatible that the Wald grammar run.
//
//
//   SetOperation
//     Select (`SELECT 1`)
//     SetOperationItem (`UNION ALL SELECT 2`)
//       SetOperationMetadata <- metadata for the first set operation
//         SetOperationType(UNION)
//         SetOperationAllOrDistinct(ALL)
//       Select
//     SetOperationItem (`INTERSECT DISTINCT SELECT 3`)
//       SetOperationMetadata <- metadata for the first set operation
//         SetOperationType(INTERSECT)
//         SetOperationAllOrDistinct(DISTINCT)
//       Select
//
// Original grammar:
//
// query_set_operation_prefix:
//     query_primary set_operation_metadata query_primary
//   | query_set_operation_prefix set_operation_metadata query_primary
//   | query_primary set_operation_metadata "FROM"
//   | query_set_operation_prefix set_operation_metadata "FROM"
// ;
//
// query_set_operation:
//     query_set_operation_prefix ;
//
// TODO(jdbrowne): make sure this divergence is respected by the update tool.
// This divergence is due to the difficulty of mutating the AST in a "past node".
// The Bison grammar can easily muate past nodes, but the Wald AST build can't.

query_set_operation_item -> SetOperationItem:
    set_operation_metadata query_primary ;

query_set_operation_items:
    query_set_operation_item
  | query_set_operation_items query_set_operation_item
;

set_operation_metadata -> SetOperationMetadata:
    opt_corresponding_outer_mode query_set_operation_type opt_hint
    all_or_distinct opt_strict opt_column_match_suffix ;

query_set_operation -> SetOperation:
    query_primary query_set_operation_items ;

query_set_operation_maybe_expression -> SetOperation:
    query_primary_maybe_expression query_set_operation_items ;

%interface QueryExpr;

query_primary -> QueryExpr:
    select
  | parenthesized_query
;

query_primary_maybe_expression:
    select
  | expression
;

select -> Select as QueryExpr:
    'SELECT' opt_hint opt_select_with opt_all_or_distinct opt_select_as_clause (select_list | %empty -> SyntaxProblem) opt_from_clause opt_clauses_following_from ;

opt_select_with:
    'WITH' identifier opt_options_list -> SelectWith
  | %empty
;

opt_select_as_clause:
    'AS' 'STRUCT'                       -> SelectAs
  | 'AS' (path_expression -> PathExpr)  -> SelectAs
  | %empty
;

extra_identifier_in_hints_name:
    'HASH'
  | 'PROTO'
  | 'PARTITION'
;

identifier_in_hints:
    identifier
  | extra_identifier_in_hints_name
;

hint_entry:
    identifier_in_hints '=' expression
  | identifier_in_hints '.' identifier_in_hints '=' expression
;

hint_with_body_prefix:
    open_integer_hint integer_literal open_hint '{' hint_entry
  | open_hint '{' hint_entry
  | hint_with_body_prefix ',' hint_entry
;

hint_with_body:
    hint_with_body_prefix '}' ;

hint -> Hint:
    open_integer_hint integer_literal
  | hint_with_body
;

# Note: this is a difference in AST shape with the original GoogleSQL AST. 
# 'ALL' or 'DISTINCT' modifiers for the SELECT AST node is a boolean on the
# Select AST class: it is not an AST node in itself.
opt_all_or_distinct:
    'ALL'       -> SelectAll
  | 'DISTINCT'  -> SelectDistinct
  | %empty
;

select_list_prefix:
    select_column
  | select_list_prefix ',' select_column
;

select_list -> SelectList:
    select_list_prefix
  | select_list_prefix ','
;

star_except_list_prefix:
    'EXCEPT' '(' identifier
  | star_except_list_prefix ',' identifier
;

star_except_list -> StarExcept:
    star_except_list_prefix ')' ;

star_replace_item -> StarReplaceItem:
    expression ('AS' identifier -> Alias) ;

// Divergence between bison_parser.y and sql.tm.
// Generated nonterms to skip:
// - star_modifiers_with_replace_prefix
// - star_modifiers
// The 2 nonterms after this comment are manually crafted, and should replace the
// skipped generated nonterm.
//
// Original grammar:
//
// star_modifiers_with_replace_prefix:
//     star_except_list 'REPLACE' '(' star_replace_item
//   | 'REPLACE' '(' star_replace_item
//   | star_modifiers_with_replace_prefix ',' star_replace_item
// ;

// star_modifiers:
//     star_except_list
//   | star_modifiers_with_replace_prefix ')'
// ;
//
// rationale: the goal is for StarReplace to NOT match the except part:
// `select * EXCEPT (dontwant) «REPLACE (oldname AS newn, badname AS cooln)»`
//
// The original API builds the AST in the recursive rule and this is incompatible
// with the textmapper arrow notations that require the full span to be avail
// in the rule.
// TODO(b/265668444): modify the Bison rule to use path_expression.

star_replace_list_prefix:
    'REPLACE' '(' star_replace_item
  | star_replace_list_prefix ',' star_replace_item
;

star_modifiers:
    star_except_list
  | star_except_list (star_replace_list_prefix ')' -> StarReplace)
  | (star_replace_list_prefix ')' -> StarReplace)
;

select_column -> SelectColumn:
    expression
  | expression ('AS' identifier -> Alias)
  | expression (identifier -> Alias)
  | expression ('.' '*' -> Star)
  | expression ('.' '*' star_modifiers -> Star)
  | ('*' -> Star)
  | ('*' star_modifiers -> Star)
  | syntax_problem
;

opt_as_alias:
    opt_as identifier -> Alias
  | %empty
;

opt_as_alias_with_required_as:
    'AS' identifier -> Alias
  | %empty
;

opt_as_or_into_alias:
    'AS' identifier -> Alias
  | ('INTO' identifier -> Into)
  | %empty
;

opt_as:
    'AS'
  | %empty
;

opt_natural:
    'NATURAL'
  | %empty
;

opt_outer:
    'OUTER'
  | %empty
;

int_literal_or_parameter:
    integer_literal
  | parameter_expression
  | system_variable_expression
;

cast_int_literal_or_parameter -> CastExpr:
    'CAST' '(' int_literal_or_parameter 'AS' type opt_format ')' ;

possibly_cast_int_literal_or_parameter:
    cast_int_literal_or_parameter
  | int_literal_or_parameter
;

repeatable_clause:
    'REPEATABLE' '(' possibly_cast_int_literal_or_parameter ')' ;

sample_size_value:
    possibly_cast_int_literal_or_parameter
  | floating_point_literal
;

sample_size_unit:
    'ROWS'
  | 'PERCENT'
;

sample_size:
    sample_size_value sample_size_unit opt_partition_by_clause_no_hint ;

opt_repeatable_clause:
    repeatable_clause
  | %empty
;

opt_sample_clause_suffix:
    repeatable_clause
  | 'WITH' 'WEIGHT' opt_repeatable_clause
  | 'WITH' 'WEIGHT' identifier opt_repeatable_clause
  | 'WITH' 'WEIGHT' 'AS' identifier opt_repeatable_clause
  | %empty
;

sample_clause -> SampleClause:
    'TABLESAMPLE' identifier '(' sample_size ')' opt_sample_clause_suffix ;

opt_sample_clause:
    sample_clause
  | %empty
;

pivot_expression -> PivotExpr:
    expression opt_as_alias ;

pivot_expression_list:
    pivot_expression
  | pivot_expression_list ',' pivot_expression
;

pivot_value -> PivotValue:
    expression opt_as_alias ;

pivot_value_list:
    pivot_value
  | pivot_value_list ',' pivot_value
;

pivot_clause:
    'PIVOT' '(' pivot_expression_list 'FOR' expression 'IN' '(' pivot_value_list ')' ')' ;

opt_as_string_or_integer:
    opt_as string_literal
  | opt_as integer_literal
  | %empty
;

path_expression_list:
    path_expression
  | path_expression_list ',' path_expression
;

path_expression_list_with_opt_parens:
    '(' path_expression_list ')'
  | path_expression
;

path_expression_list_prefix:
    '(' path_expression
  | path_expression_list_prefix ',' path_expression
;

path_expression_list_with_parens:
    path_expression_list_prefix ')' ;

opt_path_expression_list_with_parens:
    path_expression_list_with_parens
  | %empty
;

unpivot_in_item:
    path_expression_list_with_opt_parens opt_as_string_or_integer ;

unpivot_in_item_list_prefix:
    '(' unpivot_in_item
  | unpivot_in_item_list_prefix ',' unpivot_in_item
;

unpivot_in_item_list:
    unpivot_in_item_list_prefix ')' ;

opt_unpivot_nulls_filter:
    'EXCLUDE' 'NULLS'
  | 'INCLUDE' 'NULLS'
  | %empty
;

unpivot_clause:
    'UNPIVOT' opt_unpivot_nulls_filter '(' path_expression_list_with_opt_parens 'FOR' path_expression 'IN' unpivot_in_item_list ')' ;

opt_pivot_or_unpivot_clause_and_alias:
    'AS' identifier  -> Alias
  | identifier       -> Alias
  | ('AS' identifier -> Alias) (pivot_clause opt_as_alias -> PivotClause)
  | ('AS' identifier -> Alias) (unpivot_clause opt_as_alias -> UnpivotClause)
  | ('AS' identifier -> Alias) qualify_clause_nonreserved
  | (identifier -> Alias) (pivot_clause opt_as_alias -> PivotClause)
  | (identifier -> Alias) (unpivot_clause opt_as_alias -> UnpivotClause)
  | (identifier -> Alias) qualify_clause_nonreserved
  | (pivot_clause opt_as_alias -> PivotClause)
  | (unpivot_clause opt_as_alias -> UnpivotClause)
  | qualify_clause_nonreserved
  | %empty
;

table_subquery -> TableSubquery as TableExpr:
    parenthesized_query opt_pivot_or_unpivot_clause_and_alias opt_sample_clause ;

table_clause:
    'TABLE' tvf
  | 'TABLE' path_expression
;

model_clause:
    'MODEL' (path_expression -> ModelClause) ;

connection_clause:
    'CONNECTION' (path_expression_or_default -> ConnectionClause) ;

descriptor_column:
    identifier ;

descriptor_column_list:
    descriptor_column
  | descriptor_column_list ',' descriptor_column
;

descriptor_argument:
    'DESCRIPTOR' '(' descriptor_column_list ')' ;

tvf_argument -> TVFArg:
    expression
  | descriptor_argument
  | table_clause
  | model_clause
  | connection_clause
  | named_argument
  | '(' table_clause ')'       -> SyntaxProblem
  | '(' model_clause ')'       -> SyntaxProblem
  | '(' connection_clause ')'  -> SyntaxProblem
  | '(' named_argument ')'     -> SyntaxProblem
  | 'SELECT'                   -> SyntaxProblem
  | 'WITH'                     -> SyntaxProblem
;

tvf_prefix_no_args:
    // TODO(b/274063367): Wrap TVF name in PathExpr.
    path_expression '('
  | 'IF' '('
;

tvf_prefix:
    tvf_prefix_no_args tvf_argument
  | tvf_prefix ',' tvf_argument
;

tvf -> TVF as TableExpr:
    tvf_prefix_no_args ')' opt_hint opt_pivot_or_unpivot_clause_and_alias opt_sample_clause
  | tvf_prefix ')' opt_hint opt_pivot_or_unpivot_clause_and_alias opt_sample_clause
;

table_path_expression_base:
    unnest_expression
  | maybe_slashed_or_dashed_path_expression
  | path_expression '['        -> SyntaxProblem
  | path_expression '.' '('    -> SyntaxProblem
  | unnest_expression '['      -> SyntaxProblem
  | unnest_expression '.' '('  -> SyntaxProblem
;

table_path_expression -> TablePathExpr/RetainText as TableExpr:
    table_path_expression_base opt_hint opt_pivot_or_unpivot_clause_and_alias opt_with_offset_and_alias opt_at_system_time opt_sample_clause ;

%interface TableExpr;

table_primary -> TableExpr:
    tvf
  | table_path_expression
  | '(' join ')' opt_sample_clause -> ParenthesizedJoin
  | table_subquery
  | graph_table_query
;

graph_table_query -> GraphTableQuery:
    'GRAPH_TABLE' '(' path_expression 'MATCH' graph_node_pattern opt_where_clause 'COLUMNS' '(' select_list ')' ')' opt_as_alias ;

graph_node_pattern:
    '(' (opt_identifier opt_is_label_expression opt_where_clause -> GraphNodePattern) ')' ;

opt_is_label_expression:
    'IS' (label_expression -> GraphLabelFilter)
  | %empty
;

%interface GraphLabelOperation;

label_expression:
    label_primary
  | label_expression '&' label_expression        -> GraphLabelOperationAnd
  | label_expression '|' label_expression        -> GraphLabelOperationOr
  | '!' label_expression %prec UNARY_PRECEDENCE  -> GraphLabelOperationNot
;

label_primary:
    identifier  -> GraphElementLabel
  | '%'         -> GraphWildcardLabel
  | parenthesized_label_expression
;

parenthesized_label_expression -> GraphParensLabelExpr:
    '(' label_expression ')' ;

opt_at_system_time:
    'FOR' 'SYSTEM' 'TIME' 'AS' 'OF' expression
  | 'FOR' 'SYSTEM_TIME' 'AS' 'OF' expression
  | %empty
;

on_clause -> OnClause:
    'ON' expression ;

using_clause_prefix:
    'USING' '(' identifier
  | using_clause_prefix ',' identifier
;

using_clause -> UsingClause:
    using_clause_prefix ')' ;

opt_on_or_using_clause_list:
    on_or_using_clause_list
  | %empty
;

on_or_using_clause_list:
    on_or_using_clause
  | on_or_using_clause_list on_or_using_clause
;

%interface OnOrUsingClause;

on_or_using_clause -> OnOrUsingClause:
    on_clause
  | using_clause
;

opt_on_or_using_clause:
    on_or_using_clause -> OnOrUsingClauseList
  | %empty
;

join_type:
    'CROSS'
  | 'FULL' opt_outer
  | 'INNER'
  | 'LEFT' opt_outer
  | 'RIGHT' opt_outer
  | %empty
;

join_hint:
    'HASH'
  | 'LOOKUP'
  | %empty
;

join_input:
    join
  | table_primary
;

join -> Join as TableExpr:
    join_input opt_natural join_type join_hint 'JOIN' opt_hint table_primary opt_on_or_using_clause_list ;

from_clause_contents -> TableExpr:
    table_primary
  | from_clause_contents ',' table_primary                                                                          -> Join
  | from_clause_contents opt_natural join_type join_hint 'JOIN' opt_hint table_primary opt_on_or_using_clause_list  -> Join
  | '@'                                                                                                             -> SyntaxProblem
  | '?'                                                                                                             -> SyntaxProblem
  | '@@'                                                                                                            -> SyntaxProblem
;

opt_from_clause:
    from_clause
  | %empty
;

from_clause -> FromClause:
    'FROM' from_clause_contents
  | 'FROM' from_clause_contents (',' -> SyntaxProblem)
;

opt_clauses_following_from:
    where_clause opt_group_by_clause opt_having_clause opt_qualify_clause opt_window_clause
  | opt_clauses_following_where
;

opt_clauses_following_where:
    group_by_clause opt_having_clause opt_qualify_clause opt_window_clause
  | opt_clauses_following_group_by
;

opt_clauses_following_group_by:
    having_clause opt_qualify_clause opt_window_clause
  | opt_qualify_clause_reserved opt_window_clause
;

where_clause -> WhereClause:
    'WHERE' (expression | syntax_problem) ;


opt_where_clause:
    where_clause
  | %empty
;

rollup_list:
    'ROLLUP' '(' expression
  | rollup_list ',' expression
;

cube_list:
    'CUBE' '(' expression
  | cube_list ',' expression
;

grouping_set -> GroupingSet:
    '(' ')'
  | expression
  | (rollup_list ')' -> Rollup)
  | (cube_list ')' -> Cube)
;

grouping_set_list:
    'GROUPING' 'SETS' '(' grouping_set
  | grouping_set_list ',' grouping_set
;

opt_grouping_item_order:
    asc_or_desc opt_null_order -> GroupingItemOrder
  | %empty
;


grouping_item -> GroupingItem:
    '(' ')'
  | expression opt_as_alias_with_required_as opt_grouping_item_order
  | (rollup_list ')' -> Rollup)
  | (cube_list ')' -> Cube)
  | (grouping_set_list ')' -> GroupingSetList)
;

opt_and_order:
    'AND' 'ORDER'
  | %empty
;

group_by_preamble<andorder>:
    [!andorder] 'GROUP' opt_hint 'BY'
  | [andorder] 'GROUP' opt_hint opt_and_order 'BY'
;



group_by_clause_prefix<andorder>:
    group_by_preamble<andorder> (grouping_item | syntax_problem)
  | group_by_clause_prefix ',' (grouping_item | syntax_problem)
;


group_by_all -> GroupByAll:
    group_by_preamble 'ALL' ;


group_by_clause -> GroupBy:
    group_by_all
  | group_by_clause_prefix
;

opt_group_by_clause:
    group_by_clause
  | %empty
;

// flag `andorder` is only set to true by the rule `pipe_aggregate`. The effect
// is to only allow `GROUP AND ORDER BY`, in the pipe and leave it forbidden
// in the non-pipe case.
%flag andorder = false;

opt_group_by_clause_with_opt_comma<andorder>:
    group_by_clause_prefix<andorder> opt_comma -> GroupBy
  | %empty
;

having_clause -> HavingClause:
    'HAVING' (expression | syntax_problem) ;

opt_having_clause:
    having_clause
  | %empty
;

window_definition:
    identifier 'AS' window_specification ;

window_clause_prefix -> WindowClause:
    'WINDOW' window_definition
  | window_clause_prefix ',' window_definition
;

opt_window_clause:
    window_clause_prefix
  | %empty
;

opt_qualify_clause:
    qualify_clause_reserved
  | qualify_clause_nonreserved
  | %empty
;

qualify_clause_reserved -> Qualify:
    'QUALIFY' expression ;

opt_qualify_clause_reserved:
    qualify_clause_reserved
  | %empty
;

qualify_clause_nonreserved -> Qualify:
    QUALIFY_NONRESERVED expression ;


limit_offset_clause -> LimitOffset:
    'LIMIT' expression 'OFFSET' expression
  | 'LIMIT' expression
;


opt_limit_offset_clause:
    limit_offset_clause
  | %empty
;

opt_having_modifier:
    'HAVING' 'MAX' expression  -> HavingModifier
  | 'HAVING' 'MIN' expression  -> HavingModifier
  | %empty
;

opt_clamped_between_modifier:
    'CLAMPED' 'BETWEEN' expression AND_FOR_BETWEEN expression -> ClampedBetweenMod
  | %empty
;

opt_with_report_modifier:
    'WITH' 'REPORT' opt_with_report_format
  | %empty
;

opt_with_report_format:
    options_list
  | %empty
;

opt_null_handling_modifier:
    'IGNORE' 'NULLS'
  | 'RESPECT' 'NULLS'
  | %empty
;

aliased_query -> WithClauseEntry:
    identifier ('AS' | %empty -> SyntaxProblem) parenthesized_query ;

aliased_query_list:
    aliased_query
  | aliased_query_list ',' aliased_query
;

// TODO(b/265668444): guarantee the SyntaxProblem is caught for all call
// of with_clause.
with_clause:
    'WITH' aliased_query
  | 'WITH' 'RECURSIVE' aliased_query
  | with_clause ',' aliased_query
;

opt_with_clause:
    with_clause
  | %empty
;

opt_with_connection_clause:
    with_connection_clause
  | %empty
;

with_clause_with_trailing_comma -> WithClause:
    with_clause ',' ;

asc_or_desc:
    'ASC'
  | 'DESC'
;

opt_asc_or_desc:
    asc_or_desc
  | %empty
;

opt_null_order:
    'NULLS' 'FIRST'
  | 'NULLS' 'LAST'
  | %empty
;

string_literal_or_parameter:
    string_literal
  | parameter_expression
  | system_variable_expression
;

collate_clause -> Collate:
    'COLLATE' string_literal_or_parameter ;

opt_collate_clause:
    collate_clause
  | %empty
;

opt_default_collate_clause:
    'DEFAULT' collate_clause
  | %empty
;

ordering_expression -> OrderingExpr:
    expression opt_collate_clause opt_asc_or_desc opt_null_order ;

order_by_clause_prefix:
    'ORDER' opt_hint 'BY' (ordering_expression | syntax_problem)
  | order_by_clause_prefix ',' (ordering_expression | syntax_problem)
;

order_by_clause:
    order_by_clause_prefix ;

order_by_clause_with_opt_comma -> OrderBy:
    order_by_clause_prefix opt_comma ;

opt_order_by_clause:
    order_by_clause_prefix -> OrderBy
  | %empty
;

parenthesized_in_rhs:
    bare_expression_subquery
  | '(' expression ')'
  | in_list_two_or_more_prefix ')' -> InList
;

in_list_two_or_more_prefix:
    '(' expression ',' expression
  | in_list_two_or_more_prefix ',' expression
;

unnest_expression:
    'UNNEST' '(' (expression | syntax_problem) ')' -> UnnestExpr ;

unnest_expression_with_opt_alias_and_offset -> UnnestExprWithOptAliasAndOffset:
    unnest_expression opt_as_alias opt_with_offset_and_alias ;

comparative_operator:
    '='
  | '!='
  | '<>'
  | '<'
  | '<='
  | '>'
  | '>='
;

additive_operator:
    '+'
  | '-'
;

multiplicative_operator:
    '*'
  | '/'
;

shift_operator:
    '<<'
  | '>>'
;

import_type -> ImportType:
    'MODULE'
  | 'PROTO'
;

any_some_all:
    'ANY'
  | 'SOME'
  | 'ALL'
;

like_operator:
    'LIKE' %prec 'LIKE'
  | NOT_SPECIAL 'LIKE' %prec 'LIKE'
;

between_operator:
    'BETWEEN' %prec 'BETWEEN'
  | NOT_SPECIAL 'BETWEEN' %prec 'BETWEEN'
;

distinct_operator:
    'IS' 'DISTINCT' 'FROM' %prec 'DISTINCT'
  | 'IS' NOT_SPECIAL 'DISTINCT' 'FROM' %prec 'DISTINCT'
;

in_operator:
    'IN' %prec 'IN'
  | NOT_SPECIAL 'IN' %prec 'IN'
;

is_operator:
    'IS' %prec 'IS'
  | 'IS' 'NOT' %prec 'IS'
;

unary_operator:
    '+' %prec UNARY_PRECEDENCE
  | '-' %prec UNARY_PRECEDENCE
  | '~' %prec UNARY_PRECEDENCE
;

with_expression_variable:
    identifier 'AS' expression ;

with_expression_variable_prefix:
    with_expression_variable
  | with_expression_variable_prefix ',' with_expression_variable
;

with_expression -> WithExpr as Expr:
    WITH_EXPR '(' with_expression_variable_prefix ',' expression ')' ;

%interface Expr;

expression -> Expr:
    null_literal
  | boolean_literal
  | string_literal   -> Literal
  | bytes_literal    -> Literal
  | integer_literal  -> Literal
  | numeric_literal
  | bignumeric_literal
  | json_literal
  | floating_point_literal -> Literal
  | date_or_time_literal
  | range_literal
  | parameter_expression
  | system_variable_expression
  | array_constructor
  | new_constructor
  | braced_constructor
  | braced_new_constructor
  | case_expression
  | cast_expression
  | extract_expression
  | with_expression
  | replace_fields_expression
  | function_call_expression_with_clauses
  | interval_expression
  // TODO(b/263109796): Consider whether we need to handle CURRENT_* function invocations without call syntax
  | identifier -> PathExpr
  | parenthesized_expression
  | struct_constructor
  | expression_subquery
  | expression '[' expression ']' %prec PRIMARY_PRECEDENCE           -> ArraySubscriptExpr
  | expression '.' '(' path_expression ')' %prec PRIMARY_PRECEDENCE  -> ExprParensPath
  | expression '.' identifier %prec PRIMARY_PRECEDENCE               -> ExprID
  // TODO(b/263109796): Decide whether these next two should be OrExpr/AndExpr
  | expression (KW_OR -> Op) expression %prec KW_OR  -> BinaryExpr
  | expression ('AND' -> Op) expression %prec 'AND'  -> BinaryExpr
  // TODO(b/263109796): Decide whether we should distinguish UnaryOp from BinaryOp
  | ('NOT' -> Op) expression %prec UNARY_NOT_PRECEDENCE                               -> UnaryExpr
  | expression like_operator any_some_all opt_hint unnest_expression %prec 'LIKE'     -> LikeExpr
  | expression like_operator any_some_all opt_hint parenthesized_in_rhs %prec 'LIKE'  -> LikeExpr
  | expression like_operator expression %prec 'LIKE'                                  -> LikeExpr
  | expression (distinct_operator -> Op) expression %prec 'DISTINCT'                  -> BinaryExpr
  // TODO(b/263109796): Decide whether these next two should be InExpr
  | expression (in_operator -> Op) opt_hint unnest_expression %prec 'IN'               -> BinaryExpr
  | expression (in_operator -> Op) opt_hint parenthesized_in_rhs %prec 'IN'            -> BinaryExpr
  | expression between_operator expression AND_FOR_BETWEEN expression %prec 'BETWEEN'  -> BetweenExpr
  | expression (is_operator 'UNKNOWN' -> Op) %prec 'IS'                                -> UnaryExpr
  | expression (is_operator -> Op) null_literal %prec 'IS'                             -> BinaryExpr
  | expression (is_operator -> Op) boolean_literal %prec 'IS'                          -> BinaryExpr
  | expression (comparative_operator -> Op) expression %prec '='                       -> BinaryExpr
  | expression ('|' -> Op) expression                                                  -> BinaryExpr
  | expression ('^' -> Op) expression                                                  -> BinaryExpr
  | expression ('&' -> Op) expression                                                  -> BinaryExpr
  | expression ('||' -> Op) expression                                                 -> BinaryExpr
  | expression (shift_operator -> Op) expression %prec '<<'                            -> BinaryExpr
  | expression (additive_operator -> Op) expression %prec '+'                          -> BinaryExpr
  | expression (multiplicative_operator -> Op) expression %prec '*'                    -> BinaryExpr
  | (unary_operator -> Op) expression %prec UNARY_PRECEDENCE                           -> UnaryExpr
;

path_expression:
    identifier
  | path_expression '.' identifier
;

dashed_identifier:
    identifier '-' identifier
  | dashed_identifier '-' identifier
  | identifier '-' integer_literal
  | dashed_identifier '-' integer_literal
  | identifier '-' floating_point_literal identifier
  | dashed_identifier '-' floating_point_literal identifier
;

dashed_path_expression:
    dashed_identifier
  | dashed_path_expression '.' identifier
;

maybe_dashed_path_expression:
    path_expression
  | dashed_path_expression
;

// TODO(b/274063367): Consolidate with PathExpr.
//   Currently PathExpr is modeled as a nested tree of IDs while this is modeled
//   as a flat list of IDs. In the original BISON grammar, both use
//   PathExpression and are consistently mapped as a flat list of IDS.
maybe_slashed_or_dashed_path_expression -> TablePathExprBase/RetainText:
    maybe_dashed_path_expression
  | slashed_path_expression
;

slashed_identifier_separator:
    '-'
  | '/'
  | ':'
;

identifier_or_integer:
    identifier
  | integer_literal
  | script_label
;

slashed_identifier:
    '/' identifier_or_integer
  | slashed_identifier slashed_identifier_separator identifier_or_integer
  | slashed_identifier slashed_identifier_separator floating_point_literal slashed_identifier_separator identifier_or_integer
;

slashed_path_expression:
    slashed_identifier
  | slashed_identifier slashed_identifier_separator floating_point_literal identifier
  | slashed_identifier slashed_identifier_separator floating_point_literal '.' identifier
  | slashed_path_expression '.' identifier
;

array_constructor_prefix_no_expressions:
    'ARRAY' '['
  | '['
  | array_type '['
;

array_constructor_prefix:
    array_constructor_prefix_no_expressions expression
  | array_constructor_prefix ',' expression
;

array_constructor -> ArrayConstructor:
    array_constructor_prefix_no_expressions ']'
  | array_constructor_prefix ']'
;

range_literal -> RangeLiteral:
    range_type string_literal ;

date_or_time_literal_kind:
    'DATE'
  | 'DATETIME'
  | 'TIME'
  | 'TIMESTAMP'
;

date_or_time_literal -> Literal as Expr:
    date_or_time_literal_kind string_literal ;

interval_expression -> IntervalExpr:
    'INTERVAL' expression identifier
  | 'INTERVAL' expression identifier 'TO' identifier
;

parameter_expression:
    named_parameter_expression
  | '?' -> ParamExpr
;

named_parameter_expression -> ParamExpr as Expr:
    '@' identifier
  | '@' reserved_keyword_rule
;

type_name -> SimpleType:
    path_expression
  | 'INTERVAL'
;

array_type -> ArrayType:
    'ARRAY' '<' type '>' ;

struct_field -> StructField:
    identifier type
  | type
;

struct_type_prefix:
    'STRUCT' '<' struct_field
  | struct_type_prefix ',' struct_field
;

struct_type -> StructType:
    'STRUCT' '<' '>'
  | struct_type_prefix '>'
;

range_type -> RangeType:
    'RANGE' '<' type '>' ;

%interface Type;

raw_type -> Type:
    array_type
  | struct_type
  | type_name
  | range_type
;

type_parameter -> TypeParam:
    integer_literal
  | boolean_literal
  | string_literal
  | bytes_literal
  | floating_point_literal
  | 'MAX'
;

type_parameters_prefix:
    '(' type_parameter
  | type_parameters_prefix ',' type_parameter
;

opt_type_parameters:
    type_parameters_prefix ')'
  | type_parameters_prefix ',' ')'
  | %empty
;

type:
    raw_type opt_type_parameters opt_collate_clause ;

templated_parameter_kind:
    'PROTO'
  | 'ENUM'
  | 'STRUCT'
  | 'ARRAY'
  | identifier
;

templated_parameter_type -> TemplatedParamType:
    'ANY' templated_parameter_kind ;

type_or_tvf_schema:
    type
  | templated_parameter_type
  | tvf_schema
;

new_constructor_prefix_no_arg:
    'NEW' type_name '(' ;

new_constructor_prefix:
    new_constructor_prefix_no_arg new_constructor_arg
  | new_constructor_prefix ',' new_constructor_arg
;

new_constructor -> NewConstructor:
    new_constructor_prefix ')'
  | new_constructor_prefix_no_arg ')'
;

new_constructor_arg -> NewConstructorArg:
    expression
  | expression 'AS' identifier
  | expression 'AS' '(' path_expression ')'
;

// TODO(b/263109796): Decide whether we need a BracedConstructorFieldValue here.
//     It seems very redundant but is mapped on the BISON side.
braced_constructor_field_value:
    ':' expression
  | braced_constructor
;

braced_constructor_extension -> BracedConstructorField:
    '(' path_expression ')' braced_constructor_field_value ;

braced_constructor_field -> BracedConstructorField:
    identifier braced_constructor_field_value ;

braced_constructor_start:
    '{' ;

braced_constructor_prefix:
    braced_constructor_start braced_constructor_field
  | braced_constructor_start braced_constructor_extension
  | braced_constructor_prefix ',' braced_constructor_field
  | braced_constructor_prefix braced_constructor_field
  | braced_constructor_prefix ',' braced_constructor_extension
;

braced_constructor -> BracedConstructor:
    braced_constructor_start '}'
  | braced_constructor_prefix '}'
;

braced_new_constructor -> BracedNewConstructor:
    'NEW' type_name braced_constructor ;


case_no_value_expression_prefix:
    'CASE' ('WHEN' expression 'THEN' expression -> WhenThenExpr)
  | case_no_value_expression_prefix ('WHEN' expression 'THEN' expression -> WhenThenExpr)
;

case_value_expression_prefix:
    'CASE' expression ('WHEN' expression 'THEN' expression -> WhenThenExpr)
  | case_value_expression_prefix ('WHEN' expression 'THEN' expression -> WhenThenExpr)
;

case_expression_prefix:
    case_no_value_expression_prefix
  | case_value_expression_prefix
;

case_expression -> CaseExpr:
    case_expression_prefix 'END'
  | case_expression_prefix ('ELSE' expression -> ElseExpr) 'END'
;

opt_at_time_zone:
    'AT' 'TIME' 'ZONE' expression
  | %empty
;

opt_format:
    'FORMAT' expression opt_at_time_zone -> FormatClause
  | %empty
;

cast_expression -> CastExpr:
    'CAST' '(' expression 'AS' type opt_format ')'
  | 'CAST' '(' 'SELECT'
  | 'SAFE_CAST' '(' expression 'AS' type opt_format ')'
  | 'SAFE_CAST' '(' 'SELECT'
;

extract_expression_base:
    'EXTRACT' '(' expression 'FROM' expression ;

extract_expression -> ExtractExpr:
    extract_expression_base ')'
  | extract_expression_base 'AT' 'TIME' 'ZONE' expression ')'
;

replace_fields_arg -> ReplaceFieldsArg:
    expression 'AS' generalized_path_expression
  | expression 'AS' generalized_extension_path
;

replace_fields_prefix:
    'REPLACE_FIELDS' '(' expression ',' replace_fields_arg
  | replace_fields_prefix ',' replace_fields_arg
;

replace_fields_expression -> ReplaceFieldsExpr as Expr:
    replace_fields_prefix ')' ;

function_name_from_keyword -> ID:
    'IF'
  | 'GROUPING'
  | 'LEFT'
  | 'RIGHT'
  | 'COLLATE'
  | 'RANGE'
;

function_call_expression_base:
    expression '(' 'DISTINCT' %prec PRIMARY_PRECEDENCE
  | expression '(' %prec PRIMARY_PRECEDENCE
  | (function_name_from_keyword -> PathExpr) '(' %prec PRIMARY_PRECEDENCE
;

function_call_argument -> Expr:
    expression
  | 'SELECT' -> SyntaxProblem
  | named_argument
  | lambda_argument
;

named_argument -> NamedArg:
    identifier '=>' expression ;

lambda_argument -> Lambda as Expr:
    lambda_argument_list '->' expression ;

lambda_argument_list:
    expression
  | '(' ')'
;

function_call_expression_with_args_prefix:
    function_call_expression_base function_call_argument
  | function_call_expression_base '*'
  | function_call_expression_with_args_prefix ',' function_call_argument
;

function_call_expression:
    function_call_expression_base opt_having_modifier opt_order_by_clause opt_limit_offset_clause ')'
  | function_call_expression_with_args_prefix opt_null_handling_modifier opt_having_modifier opt_clamped_between_modifier opt_with_report_modifier opt_order_by_clause opt_limit_offset_clause ')'
;

opt_identifier:
    identifier
  | %empty
;

partition_by_clause_prefix -> PartitionBy:
    'PARTITION' opt_hint 'BY' expression
  | partition_by_clause_prefix ',' expression
;

opt_partition_by_clause:
    partition_by_clause_prefix
  | %empty
;

partition_by_clause_prefix_no_hint:
    'PARTITION' 'BY' expression
  | partition_by_clause_prefix_no_hint ',' expression
;

opt_partition_by_clause_no_hint:
    partition_by_clause_prefix_no_hint -> PartitionBy
  | %empty
;

cluster_by_clause_prefix_no_hint:
    'CLUSTER' 'BY' expression
  | cluster_by_clause_prefix_no_hint ',' expression
;

opt_cluster_by_clause_no_hint:
    cluster_by_clause_prefix_no_hint
  | %empty
;

opt_ttl_clause:
    'ROW' 'DELETION' 'POLICY' '(' expression ')' -> TtlClause
  | %empty
;

preceding_or_following:
    'PRECEDING'
  | 'FOLLOWING'
;

window_frame_bound:
    'UNBOUNDED' preceding_or_following
  | 'CURRENT' 'ROW'
  | expression preceding_or_following
;

frame_unit:
    'ROWS'
  | 'RANGE'
;

opt_window_frame_clause:
    frame_unit 'BETWEEN' window_frame_bound AND_FOR_BETWEEN window_frame_bound  -> WindowFrame
  | frame_unit window_frame_bound                                               -> WindowFrame
  | %empty
;

window_specification:
    identifier
  | '(' opt_identifier opt_partition_by_clause opt_order_by_clause opt_window_frame_clause ')'
;

function_call_expression_with_clauses -> FuncCallExpr as Expr:
    function_call_expression opt_hint opt_with_group_rows opt_over_clause ;

opt_with_group_rows:
    'WITH' 'GROUP_ROWS' parenthesized_query
  | WITH_EXPR
  | %empty
;

opt_over_clause:
    'OVER' window_specification -> OverClause
  | %empty
;

parenthesized_expression:
    '(' (expression | syntax_problem) ')' ;

struct_constructor_prefix_with_keyword:
    struct_constructor_prefix_with_keyword_no_arg struct_constructor_arg
  | struct_constructor_prefix_with_keyword ',' struct_constructor_arg
;

struct_constructor_prefix_without_keyword:
    '(' (expression -> StructConstructorArg) ',' (expression -> StructConstructorArg)
  | struct_constructor_prefix_without_keyword ',' (expression -> StructConstructorArg)
;

struct_constructor -> StructConstructorExpr as Expr:
    struct_constructor_prefix_with_keyword ')'
  | struct_constructor_prefix_with_keyword_no_arg ')'
  | struct_constructor_prefix_without_keyword ')'
;

struct_constructor_prefix_with_keyword_no_arg:
    struct_type '('
  | 'STRUCT' '('
;

struct_constructor_arg -> StructConstructorArg:
    expression opt_as_alias_with_required_as ;

expression_subquery -> SubqueryExpr as Expr:
    'ARRAY' parenthesized_query
  | 'EXISTS' opt_hint parenthesized_query
  | bare_expression_subquery
;

bare_expression_subquery:
    '(' query_maybe_expression ')' ;

null_literal -> Literal as Expr:
    'NULL' ;

boolean_literal -> Literal as Expr:
    'TRUE'
  | 'FALSE'
;

numeric_literal_prefix:
    'NUMERIC'
  | 'DECIMAL'
;

numeric_literal -> Literal as Expr:
    numeric_literal_prefix string_literal ;

bignumeric_literal_prefix:
    'BIGNUMERIC'
  | 'BIGDECIMAL'
;

bignumeric_literal -> Literal as Expr:
    bignumeric_literal_prefix string_literal ;

json_literal -> Literal as Expr:
    'JSON' string_literal ;

identifier -> ID/RetainText:
    identifier_in_lexer
  | keyword_as_identifier
;

label:
    script_label ;

system_variable_expression -> SystemVarExpr as Expr:
    '@@' (path_expression -> PathExpr) %prec DOUBLE_AT_PRECEDENCE
  | '@@' (reserved_keyword_rule -> PathExpr)
;

%generate reservedKeywords = set(reserved_keyword_rule);

// Reserved keywords
reserved_keyword_rule:
    // go/keep-sorted start
    'ALL'
  | 'AND'
  | 'ANY'
  | 'ARRAY'
  | 'AS'
  | 'ASC'
  | 'ASSERT_ROWS_MODIFIED'
  | 'AT'
  | 'BETWEEN'
  | 'BY'
  | 'CASE'
  | 'CAST'
  | 'COLLATE'
  | 'CONTAINS'
  | 'CREATE'
  | 'CROSS'
  | 'CUBE'
  | 'CURRENT'
  | 'DEFAULT'
  | 'DEFINE'
  | 'DESC'
  | 'DISTINCT'
  | 'ELSE'
  | 'END'
  | 'ENUM'
  | 'ESCAPE'
  | 'EXCEPT'
  | 'EXCLUDE'
  | 'EXISTS'
  | 'EXTRACT'
  | 'FALSE'
  | 'FETCH'
  | 'FOLLOWING'
  | 'FOR'
  | 'FROM'
  | 'FULL'
  | 'GRAPH_TABLE'
  | 'GROUP'
  | 'GROUPING'
  | 'GROUPS'
  | 'HASH'
  | 'HAVING'
  | 'IF'
  | 'IGNORE'
  | 'IN'
  | 'INNER'
  | 'INTERSECT'
  | 'INTERVAL'
  | 'INTO'
  | 'IS'
  | 'JOIN'
  | 'LATERAL'
  | 'LEFT'
  | 'LIKE'
  | 'LIMIT'
  | 'LOOKUP'
  | 'MERGE'
  | 'NATURAL'
  | 'NEW'
  | 'NO'
  | 'NOT'
  | 'NULL'
  | 'NULLS'
  | 'OF'
  | 'ON'
  | 'ORDER'
  | 'OUTER'
  | 'OVER'
  | 'PARTITION'
  | 'PRECEDING'
  | 'PROTO'
  | 'QUALIFY'
  | 'RANGE'
  | 'RECURSIVE'
  | 'RESPECT'
  | 'RIGHT'
  | 'ROLLUP'
  | 'ROWS'
  | 'SELECT'
  | 'SET'
  | 'SOME'
  | 'STRUCT'
  | 'TABLESAMPLE'
  | 'THEN'
  | 'TO'
  | 'TREAT'
  | 'TRUE'
  | 'UNBOUNDED'
  | 'UNION'
  | 'UNNEST'
  | 'USING'
  | 'WHEN'
  | 'WHERE'
  | 'WINDOW'
  | 'WITH'
  | 'WITHIN'
  | KW_OR
  // go/keep-sorted end
;

// Soft keywords a.k.a unreserved keywords
keyword_as_identifier:
    'ABORT'
  | 'ACCESS'
  | 'ACTION'
  | 'AGGREGATE'
  | 'ADD'
  | 'ALTER'
  | 'ALWAYS'
  | 'ANALYZE'
  | 'ARE'
  | 'ASCENDING'
  | 'ASSERT'
  | 'BATCH'
  | 'BEGIN'
  | 'BIGDECIMAL'
  | 'BIGNUMERIC'
  | 'BREAK'
  | 'CALL'
  | 'CASCADE'
  | 'CHECK'
  | 'CLAMPED'
  | 'CLONE'
  | 'COPY'
  | 'CLUSTER'
  | 'COLUMN'
  | 'COLUMNS'
  | 'COMMIT'
  | 'CONNECTION'
  | 'CONSTANT'
  | 'CONSTRAINT'
  | 'CONTINUE'
  | 'CORRESPONDING'
  | 'CYCLE'
  | 'DATA'
  | 'DATABASE'
  | 'DATE'
  | 'DATETIME'
  | 'DECIMAL'
  | 'DECLARE'
  | 'DEFINER'
  | 'DELETE'
  | 'DELETION'
  | 'DESCENDING'
  | 'DESCRIBE'
  | 'DESTINATION'
  | 'DETERMINISTIC'
  | 'DO'
  | 'DROP'
  | 'EDGE'
  | 'ELSEIF'
  | 'ENFORCED'
  | KW_ERROR
  | 'EXCEPTION'
  | 'EXECUTE'
  | 'EXPLAIN'
  | 'EXPORT'
  | 'EXTEND'
  | 'EXTERNAL'
  | 'FILES'
  | 'FILTER'
  | 'FILL'
  | 'FIRST'
  | 'FOREIGN'
  | 'FORMAT'
  | 'FUNCTION'
  | 'GENERATED'
  | 'GRANT'
  | 'GRAPH'
  | 'GROUP_ROWS'
  | 'HIDDEN'
  | 'IDENTITY'
  | 'IMMEDIATE'
  | 'IMMUTABLE'
  | 'IMPORT'
  | 'INCLUDE'
  | 'INCREMENT'
  | 'INDEX'
  | 'INOUT'
  | 'INPUT'
  | 'INSERT'
  | 'INVOKER'
  | 'ISOLATION'
  | 'ITERATE'
  | 'JSON'
  | 'KEY'
  | 'LABEL'
  | 'LANGUAGE'
  | 'LAST'
  | 'LEAVE'
  | 'LEVEL'
  | 'LOAD'
  | 'LOG'
  | 'LOOP'
  | 'MATCH'
  | 'MATCHED'
  | 'MATERIALIZED'
  | 'MAX'
  | 'MAXVALUE'
  | 'MESSAGE'
  | 'MIN'
  | 'MINVALUE'
  | 'MODEL'
  | 'MODULE'
  | 'NEXT'
  | 'NODE'
  | 'NUMERIC'
  | 'OFFSET'
  | 'ONLY'
  | 'OPTIONAL'
  | 'OPTIONS'
  | 'OUT'
  | 'OUTPUT'
  | 'OVERWRITE'
  | 'PARTITIONS'
  | 'PERCENT'
  | 'PIVOT'
  | 'POLICIES'
  | 'POLICY'
  | 'PRIMARY'
  | 'PRIVATE'
  | 'PRIVILEGE'
  | 'PRIVILEGES'
  | 'PROCEDURE'
  | 'PROJECT'
  | 'PROPERTIES'
  | 'PROPERTY'
  | 'PUBLIC'
  | QUALIFY_NONRESERVED
  | 'RAISE'
  | 'READ'
  | 'REFERENCES'
  | 'REMOTE'
  | 'REMOVE'
  | 'RENAME'
  | 'REPEAT'
  | 'REPEATABLE'
  | 'REPLACE'
  | 'REPLACE_FIELDS'
  | 'REPLICA'
  | 'REPORT'
  | 'RESTRICT'
  | 'RESTRICTION'
  | 'RETURNS'
  | 'RETURN'
  | 'REVOKE'
  | 'ROLLBACK'
  | 'ROW'
  | 'RUN'
  | 'SAFE_CAST'
  | 'SCHEMA'
  | 'SEARCH'
  | 'SECURITY'
  | 'SEQUENCE'
  | 'SETS'
  | 'SHOW'
  | 'SIMPLE'
  | 'SNAPSHOT'
  | 'SOURCE'
  | 'SQL'
  | 'STABLE'
  | 'START'
  | 'STATIC_DESCRIBE'
  | 'STORED'
  | 'STORING'
  | 'STRICT'
  | 'SYSTEM'
  | 'SYSTEM_TIME'
  | 'TABLE'
  | 'TARGET'
  | 'TEMP'
  | 'TEMPORARY'
  | 'TIME'
  | 'TIMESTAMP'
  | 'TRANSACTION'
  | 'TRANSFORM'
  | 'TRUNCATE'
  | 'TYPE'
  | 'UNDROP'
  | 'UNIQUE'
  | 'UNKNOWN'
  | 'UNPIVOT'
  | 'UNTIL'
  | 'UPDATE'
  | 'VALUE'
  | 'VALUES'
  | 'VECTOR'
  | 'VIEW'
  | 'VIEWS'
  | 'VOLATILE'
  | 'WEIGHT'
  | 'WHILE'
  | 'WRITE'
  | 'ZONE'
  | 'DESCRIPTOR'
  | 'INTERLEAVE'
  | 'NULL_FILTERED'
  | 'PARENT'
;

opt_or_replace:
    KW_OR 'REPLACE'
  | %empty
;

opt_create_scope:
    'TEMP'       -> Modifier/RetainText
  | 'TEMPORARY'  -> Modifier/RetainText
  | 'PUBLIC'     -> Modifier/RetainText
  | 'PRIVATE'    -> Modifier/RetainText
  | %empty
;

opt_unique:
    'UNIQUE'
  | %empty
;

opt_search:
    'SEARCH'
  | %empty
;

describe_keyword:
    'DESCRIBE'
  | 'DESC'
;

opt_hint:
    hint
  | %empty
;

options_entry -> OptionEntry:
    identifier_in_hints '=' expression_or_proto ;

expression_or_proto:
    'PROTO'
  | expression
;

options_list_prefix:
    '(' options_entry
  | options_list_prefix ',' options_entry
;

options_list:
    options_list_prefix ')'
  | '(' ')'
;

opt_options_list:
    ('OPTIONS' options_list)
  | %empty
;

define_table_statement -> DefineTableStmt:
    'DEFINE' 'TABLE' (path_expression -> TableName/RetainText) options_list ;

dml_statement:
    insert_statement
  | delete_statement
  | update_statement
;

opt_from_keyword:
    'FROM'
  | %empty
;

opt_where_expression:
    'WHERE' expression
  | %empty
;

opt_assert_rows_modified:
    'ASSERT_ROWS_MODIFIED' possibly_cast_int_literal_or_parameter
  | %empty
;

opt_returning_clause:
    'THEN' 'RETURN' select_list
  | 'THEN' 'RETURN' 'WITH' 'ACTION' select_list
  | 'THEN' 'RETURN' 'WITH' 'ACTION' 'AS' identifier select_list
  | %empty
;

unambiguous_or_ignore_replace_update:
    KW_OR 'IGNORE'
  | 'IGNORE'
  | KW_OR 'REPLACE'
  | KW_OR 'UPDATE'
;

// TODO(b/246408133): Correctly map INSERT INTO statements.
//
// The grammar rules translated from BISON are overly lenient and are
// disambiguated on the BISON side in the generated C++ code. We just map them
// to an arbitrary node to make the whole grammar parseable but need to fix this
// at some point.
//
// This is also solvable via lazy "view" objects in Go that wrap a specific node
// in an AST and give convenient access to its content. Basically, for the sake
// of parser performance and simplicity we can push some problems and complexity
// into the analysis phase. e.g.:
//
//   type insertStmt struct {
//     ast.Node
//   }
//
//   func (is insertStmt) Values() []ast.Node {
//      return ....
//   }
//
insert_statement_prefix -> InsertStmtPart:
    'INSERT'
  | insert_statement_prefix unambiguous_or_ignore_replace_update
  | insert_statement_prefix 'INTO' maybe_dashed_generalized_path_expression opt_hint
  | insert_statement_prefix generalized_path_expression opt_hint
  | insert_statement_prefix column_list
  | insert_statement_prefix 'VALUES' insert_values_list
;

insert_statement -> InsertStmt as Stmt:
    insert_statement_prefix opt_assert_rows_modified opt_returning_clause
  | insert_statement_prefix (query -> Query) opt_assert_rows_modified opt_returning_clause
;

copy_data_source:
    maybe_dashed_path_expression opt_at_system_time opt_where_clause ;

clone_data_source -> CloneDataSource:
    maybe_dashed_path_expression opt_at_system_time opt_where_clause ;

clone_data_source_list:
    clone_data_source
  | clone_data_source_list 'UNION' 'ALL' clone_data_source
;

clone_data_statement -> CloneDataStmt:
    'CLONE' 'DATA' 'INTO' maybe_dashed_path_expression 'FROM' clone_data_source_list ;

expression_or_default:
    expression
  | 'DEFAULT'
;

insert_values_row_prefix:
    '(' expression_or_default
  | insert_values_row_prefix ',' expression_or_default
;

insert_values_row:
    insert_values_row_prefix ')' ;

insert_values_list:
    insert_values_row
  | insert_values_list ',' insert_values_row
;

delete_statement -> DeleteStmt as Stmt:
    'DELETE' opt_from_keyword maybe_dashed_generalized_path_expression opt_hint opt_as_alias opt_with_offset_and_alias opt_where_expression opt_assert_rows_modified opt_returning_clause ;

opt_with_offset_and_alias:
    'WITH' 'OFFSET' opt_as_alias -> WithOffset
  | %empty
;

update_statement -> UpdateStmt as Stmt:
    'UPDATE' maybe_dashed_generalized_path_expression opt_hint opt_as_alias opt_with_offset_and_alias 'SET' update_item_list opt_from_clause opt_where_expression opt_assert_rows_modified opt_returning_clause ;

truncate_statement -> TruncateStmt:
    'TRUNCATE' 'TABLE' maybe_dashed_path_expression opt_where_expression ;

nested_dml_statement:
    '(' dml_statement ')' ;

generalized_path_expression -> PathExpr:
    identifier
  | generalized_path_expression '.' generalized_extension_path
  | generalized_path_expression '.' identifier
  | generalized_path_expression '[' expression ']'
;

maybe_dashed_generalized_path_expression:
    generalized_path_expression
  | dashed_path_expression
;

generalized_extension_path:
    '(' path_expression ')'
  | generalized_extension_path '.' '(' path_expression ')'
  | generalized_extension_path '.' identifier
;

update_set_value -> UpdateSetValue:
    generalized_path_expression '=' expression_or_default ;

%interface UpdateItem;

update_item -> UpdateItem:
    update_set_value
  | nested_dml_statement
;

update_item_list:
    update_item
  | update_item_list ',' update_item
;

opt_into:
    'INTO'
  | %empty
;

opt_by_target:
    'BY' 'TARGET'
  | %empty
;

opt_and_expression:
    'AND' expression
  | %empty
;

merge_insert_value_list_or_source_row:
    'VALUES' insert_values_row
  | 'ROW'
;

merge_action:
    'INSERT' opt_column_list merge_insert_value_list_or_source_row
  | 'UPDATE' 'SET' update_item_list
  | 'DELETE'
;

merge_when_clause -> MergeWhenClause:
    'WHEN' 'MATCHED' opt_and_expression 'THEN' merge_action
  | 'WHEN' 'NOT' 'MATCHED' opt_by_target opt_and_expression 'THEN' merge_action
  | 'WHEN' 'NOT' 'MATCHED' 'BY' 'SOURCE' opt_and_expression 'THEN' merge_action
;

merge_when_clause_list:
    merge_when_clause
  | merge_when_clause_list merge_when_clause
;

merge_source:
    table_path_expression
  | table_subquery
;

merge_statement_prefix:
    'MERGE' opt_into maybe_dashed_path_expression opt_as_alias 'USING' merge_source 'ON' table=expression ;

merge_statement -> MergeStmt:
    merge_statement_prefix merge_when_clause_list ;

call_statement_with_args_prefix:
    'CALL' (path_expression -> CallName) '(' tvf_argument
  | call_statement_with_args_prefix ',' tvf_argument
;

call_statement -> CallStmt:
    call_statement_with_args_prefix ')'
  | 'CALL' (path_expression -> CallName) '(' ')'
;

opt_function_parameters:
    function_parameters
  | %empty
;

opt_if_exists:
    'IF' 'EXISTS'
  | %empty
;

opt_access:
    'ACCESS'
  | %empty
;

drop_all_row_access_policies_statement -> DropAllRowAccessPoliciesStmt:
    'DROP' 'ALL' 'ROW' opt_access 'POLICIES' 'ON' path_expression ;

on_path_expression:
    'ON' path_expression ;

opt_on_path_expression:
    'ON' path_expression
  | %empty
;

opt_drop_mode:
    'RESTRICT'
  | 'CASCADE'
  | %empty
;

drop_statement -> DropStmt:
    'DROP' 'PRIVILEGE' 'RESTRICTION' opt_if_exists 'ON' privilege_list 'ON' identifier path_expression
  | 'DROP' 'ROW' 'ACCESS' 'POLICY' opt_if_exists identifier on_path_expression
  | 'DROP' 'SEARCH' 'INDEX' opt_if_exists path_expression opt_on_path_expression
  | 'DROP' table_or_table_function opt_if_exists maybe_dashed_path_expression opt_function_parameters
  | 'DROP' 'SNAPSHOT' 'TABLE' opt_if_exists maybe_dashed_path_expression
  | 'DROP' generic_entity_type opt_if_exists path_expression
  | 'DROP' schema_object_kind opt_if_exists path_expression opt_function_parameters opt_drop_mode
;

non_empty_statement_list:
    terminated_statement
  | non_empty_statement_list terminated_statement
;

unterminated_non_empty_statement_list:
    unterminated_statement
  | non_empty_statement_list unterminated_statement
;

opt_execute_into_clause:
    'INTO' identifier_list
  | %empty
;

execute_using_argument:
    expression 'AS' identifier
  | expression
;

execute_using_argument_list:
    execute_using_argument
  | execute_using_argument_list ',' execute_using_argument
;

opt_execute_using_clause:
    'USING' execute_using_argument_list
  | %empty
;

execute_immediate -> ExecuteImmediateStmt:
    'EXECUTE' 'IMMEDIATE' expression opt_execute_into_clause opt_execute_using_clause ;

script -> File/FileFlags:
    non_empty_statement_list
  | unterminated_non_empty_statement_list
  | %empty
;

statement_list:
    non_empty_statement_list
  | %empty
;

opt_else:
    'ELSE' statement_list
  | %empty
;

elseif_clauses:
    'ELSEIF' expression 'THEN' statement_list -> ElseIfClause
  | elseif_clauses ('ELSEIF' expression 'THEN' statement_list -> ElseIfClause)
;

opt_elseif_clauses:
    elseif_clauses
  | %empty
;

if_statement_unclosed:
    'IF' expression 'THEN' statement_list opt_elseif_clauses opt_else ;

if_statement -> IfStmt:
    if_statement_unclosed 'END' 'IF' ;

when_then_clauses:
    'WHEN' expression 'THEN' statement_list -> WhenThenClause
  | when_then_clauses ('WHEN' expression 'THEN' statement_list -> WhenThenClause)
;

opt_expression:
    expression
  | %empty
;

case_statement -> CaseStmt:
    'CASE' opt_expression when_then_clauses opt_else 'END' 'CASE' ;

unlabeled_begin_end_block:
    'BEGIN' statement_list opt_exception_handler 'END' ;

begin_end_block -> BeginEndBlockStmt:
    unlabeled_begin_end_block
  | label ':' unlabeled_begin_end_block opt_identifier
;

opt_exception_handler:
    'EXCEPTION' 'WHEN' KW_ERROR 'THEN' statement_list
  | %empty
;

opt_default_expression:
    'DEFAULT' expression
  | %empty
;

identifier_list:
    identifier
  | identifier_list ',' identifier
;

variable_declaration -> DeclareStmt:
    'DECLARE' identifier_list type opt_default_expression
  | 'DECLARE' identifier_list 'DEFAULT' expression
;

unlabeled_loop_statement:
    'LOOP' statement_list 'END' 'LOOP' ;

loop_statement -> LoopStmt:
    unlabeled_loop_statement
  | label ':' unlabeled_loop_statement opt_identifier
;

unlabeled_while_statement:
    'WHILE' expression 'DO' statement_list 'END' 'WHILE' ;

while_statement -> WhileStmt:
    unlabeled_while_statement
  | label ':' unlabeled_while_statement opt_identifier
;

until_clause:
    'UNTIL' expression ;

unlabeled_repeat_statement:
    'REPEAT' statement_list until_clause 'END' 'REPEAT' ;

repeat_statement -> RepeatStmt:
    unlabeled_repeat_statement
  | label ':' unlabeled_repeat_statement opt_identifier
;

unlabeled_for_in_statement:
    'FOR' identifier 'IN' parenthesized_query 'DO' statement_list 'END' 'FOR' ;

for_in_statement -> ForInStmt:
    unlabeled_for_in_statement
  | label ':' unlabeled_for_in_statement opt_identifier
;

break_statement -> BreakStmt:
    'BREAK' opt_identifier
  | 'LEAVE' opt_identifier
;

continue_statement -> ContinueStmt:
    'CONTINUE' opt_identifier
  | 'ITERATE' opt_identifier
;

return_statement -> ReturnStmt:
    'RETURN' ;

raise_statement -> RaiseStmt:
    'RAISE'
  | 'RAISE' 'USING' 'MESSAGE' '=' expression
;

next_statement_kind:
    opt_hint next_statement_kind_without_hint ;

next_statement_kind_parenthesized_select:
    '(' next_statement_kind_parenthesized_select
  | 'SELECT'
  | 'WITH'
;

next_statement_kind_table:
    'TABLE' ;

next_statement_kind_create_table_opt_as_or_semicolon:
    'AS'
  | ';'
  | %empty
;

next_statement_kind_create_modifiers:
    opt_or_replace opt_create_scope ;

next_statement_kind_without_hint:
    'EXPLAIN'
  | next_statement_kind_parenthesized_select
  | 'DEFINE' 'TABLE'
  | 'EXECUTE' 'IMMEDIATE'
  | 'EXPORT' 'DATA'
  | 'EXPORT' 'MODEL'
  | 'INSERT'
  | 'UPDATE'
  | 'DELETE'
  | 'MERGE'
  | 'CLONE' 'DATA'
  | 'LOAD' 'DATA'
  | describe_keyword
  | 'SHOW'
  | 'DROP' 'PRIVILEGE'
  | 'DROP' 'ALL' 'ROW' opt_access 'POLICIES'
  | 'DROP' 'ROW' 'ACCESS' 'POLICY'
  | 'DROP' 'SEARCH' 'INDEX'
  | 'DROP' table_or_table_function
  | 'DROP' 'SNAPSHOT' 'TABLE'
  | 'DROP' generic_entity_type
  | 'DROP' schema_object_kind
  | 'GRANT'
  | 'REVOKE'
  | 'RENAME'
  | 'START'
  | 'BEGIN'
  | 'SET' 'TRANSACTION' identifier
  | 'SET' identifier '='
  | 'SET' named_parameter_expression '='
  | 'SET' system_variable_expression '='
  | 'SET' '('
  | 'COMMIT'
  | 'ROLLBACK'
  | 'START' 'BATCH'
  | 'RUN' 'BATCH'
  | 'ABORT' 'BATCH'
  | 'ALTER' 'DATABASE'
  | 'ALTER' 'SCHEMA'
  | 'ALTER' 'TABLE'
  | 'ALTER' 'PRIVILEGE'
  | 'ALTER' 'ROW'
  | 'ALTER' 'ALL' 'ROW' 'ACCESS' 'POLICIES'
  | 'ALTER' 'VIEW'
  | 'ALTER' 'MATERIALIZED' 'VIEW'
  | 'ALTER' generic_entity_type
  | 'ALTER' 'MODEL'
  | 'CREATE' 'DATABASE'
  | 'CREATE' next_statement_kind_create_modifiers opt_aggregate 'CONSTANT'
  | 'CREATE' next_statement_kind_create_modifiers opt_aggregate 'FUNCTION'
  | 'CREATE' next_statement_kind_create_modifiers 'PROCEDURE'
  | 'CREATE' opt_or_replace opt_unique opt_spanner_null_filtered opt_search 'INDEX'
  | 'CREATE' opt_or_replace 'SCHEMA'
  | 'CREATE' opt_or_replace generic_entity_type
  | 'CREATE' next_statement_kind_create_modifiers next_statement_kind_table opt_if_not_exists maybe_dashed_path_expression opt_table_element_list opt_like_path_expression opt_clone_table opt_copy_table opt_default_collate_clause opt_partition_by_clause_no_hint opt_cluster_by_clause_no_hint opt_options_list next_statement_kind_create_table_opt_as_or_semicolon -> CreateTableStmt
  | 'CREATE' next_statement_kind_create_modifiers 'MODEL'
  | 'CREATE' next_statement_kind_create_modifiers 'TABLE' 'FUNCTION'
  | 'CREATE' next_statement_kind_create_modifiers 'EXTERNAL'
  | 'CREATE' opt_or_replace 'PRIVILEGE'
  | 'CREATE' opt_or_replace 'ROW' opt_access 'POLICY'
  | 'CREATE' next_statement_kind_create_modifiers opt_recursive 'VIEW'
  | 'CREATE' opt_or_replace 'MATERIALIZED' opt_recursive 'VIEW'
  | 'CREATE' opt_or_replace 'SNAPSHOT' 'TABLE'
  | 'CALL'
  | 'RETURN'
  | 'IMPORT'
  | 'MODULE'
  | 'ANALYZE'
  | 'ASSERT'
  | 'TRUNCATE'
  | 'IF'
  | 'WHILE'
  | 'LOOP'
  | 'DECLARE'
  | 'BREAK'
  | 'LEAVE'
  | 'CONTINUE'
  | 'ITERATE'
  | 'RAISE'
  | 'FOR'
  | 'REPEAT'
  | label ':' 'BEGIN'
  | label ':' 'LOOP'
  | label ':' 'WHILE'
  | label ':' 'FOR'
  | label ':' 'REPEAT'
;

spanner_primary_key -> PrimaryKey:
    'PRIMARY' 'KEY' primary_key_element_list ;

opt_spanner_index_interleave_clause:
    ',' 'INTERLEAVE' 'IN' maybe_dashed_path_expression
  | %empty
;

opt_spanner_interleave_in_parent_clause:
    ',' 'INTERLEAVE' 'IN' 'PARENT' maybe_dashed_path_expression opt_foreign_key_on_delete
  | %empty
;

opt_spanner_table_options:
    spanner_primary_key opt_spanner_interleave_in_parent_clause -> SpannerTableOptions
  | %empty
;

opt_spanner_null_filtered:
    'NULL_FILTERED'
  | %empty
;

spanner_generated_or_default -> GeneratedColumnInfo:
    'AS' '(' expression ')' 'STORED'
  | default_column_info
;

opt_spanner_generated_or_default:
    spanner_generated_or_default
  | %empty
;

opt_spanner_not_null_attribute:
    not_null_column_attribute
  | %empty
;

spanner_alter_column_action -> SpannerAlterColumnAction:
    'ALTER' 'COLUMN' opt_if_exists identifier column_schema_inner opt_spanner_not_null_attribute opt_spanner_generated_or_default opt_options_list ;

spanner_set_on_delete_action -> SpannerSetOnDeleteAction:
    'SET' 'ON' 'DELETE' foreign_key_action ;



// bison_parser.y END
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

syntax_problem -> SyntaxProblem:
    error ;

invalid_statement -> Stmt:
    syntax_problem ;

// Support macros.

extend keyword_as_identifier:
    'MACRO' ;

extend sql_statement_body:
    define_macro_statement ;

extend next_statement_kind_without_hint:
    'DEFINE' 'MACRO' ;

// This extension of the identifier is helpful to produce an ID AST node whenever
// there is a macro call. At the same time, this is really limited as macro
// calls can possibly occur everywhere unlike identifiers. These two productions
// only catch a very small subset of the macro calls.
extend identifier:
    '$' identifier          -> MacroCall
  | '$' '{' identifier '}'  -> MacroCall
;

macro_identifier -> ID/RetainText:
    identifier_in_lexer
  | keyword_as_identifier
  | reserved_keyword_rule
;

// Wald grammar macro support differs from the Bison grammar: in Bison, all
// tokens in a macro have an identical token type: MACRO_BODY_TOKEN. In Wald, it
// is more useful to keep the common SQL token types.
define_macro_statement -> DefineMacroStmt:
    'DEFINE' 'MACRO' (macro_identifier -> MacroName) (set(~(eoi | ';'))+ -> MacroBody) ;


// RUN statements in Plx and SQL Binary

extend sql_statement_body:
    run_script_statement ;

extend next_statement_kind_without_hint:
    'RUN' ;

run_script_statement -> RunStmt:
    'RUN' run_script_name '(' run_script_parameter_list? ')'
  | 'RUN' '[' run_script_name ']' '(' run_script_parameter_list? ')'
  | 'RUN' ((string_literal -> ID/RetainText) -> RunPath)
;

run_script_name -> RunID:
    ((run_script_name_segment '::')? run_script_name_segment ('.' run_script_name_segment)* -> ID/RetainText) ;

run_script_name_segment:
    identifier_in_lexer
  | keyword_as_identifier
;

run_script_parameter_list:
    run_parameter
  | run_script_parameter_list ',' run_parameter
;

run_parameter -> RunParam:
    identifier '=' (string_literal -> RunParamValue) ;

// The section after %% overrides templates in http://cs/go_lexer.go.tmpl
//
// If you need to add code running during the semantic actions, prefer adding
// the code to semantic_actions.go which lives in the same syntax package as
// the generated lexer.
// Small restriction, this is only possible for code that does not need to be
// inserted into generated code that uses the input data.
%%

{{define "stateVars"}}
  // Stack of start conditions to support dotIdentifier, inBetween or
  // inArrayOrStruct. E.g. when we have '<' after 'ARRAY' keyword then parser
  // should enter a separate mode after encountering '<' till it hits a '>'
  // token. `stack` holds all the state/start conditions, except the current
  // which is in State.
  _stack: number[];
  _previousToken: {{template "tokenType"}};
{{end}}

{{define "initStateVars"}}
    this._stack = [];
    this._previousToken = {{template "tokenType"}}.UNAVAILABLE;
{{end}}

{{define "onAfterNext"}}
        this._previousToken = tok;
{{end}}

{{define "onBeforeLexer"}}
// soft keywords can be used as identifier (unreserved keywords)
const softKeywordStart = {{template "tokenType"}}.ABORT;
const softKeywordEnd = {{template "tokenType"}}.ZONE + 1;

// isSoft returns true when a given token is a `soft keyword` a.k.a. `unreserved keyword“.
// An identifier name can be a soft keyword.
function isSoft(tok: {{template "tokenType"}}) : boolean {
  return tok >= softKeywordStart && tok < softKeywordEnd;
}

{{end}}

{{define "lexerExtras"}}
  // isAtDotAfterClosingBracketOrSoftKeyword checks that the current character is a dot and
  // the previous token is either:
  // - a proper identifier [a-zA-Z_][a-zA-Z0-9_] or quote identifier.
  // - or a closing parentheses, a closing bracket or a question mark
  // - or is a soft keyword.
  isAtDotAfterClosingBracketOrSoftKeyword() : boolean {
    if (this._source.charAt(this._tokenOffset) !== '.') {
      return false;
    }
    let prev = this._previousToken;
    return (prev == {{template "tokenType" }}.IDENTIFIER_IN_LEXER ||
      prev == {{template "tokenType" }}.RPAREN ||
      prev == {{template "tokenType" }}.RBRACK ||
      prev == {{template "tokenType" }}.QUEST ||
      isSoft(prev));
  } 

  push(newState: number) : void {
    this._stack.push(this._state);
    this._state = newState;
  }

  // pop replaces the lexer State with the state at the top of the stack.
  // This is a noop when the stack is empty.
  pop() : void {
    if (this._stack.length === 0) {
      return // Typically, unbalanced (too many closing) brackets will try to pop() an empty stack.
    }
    this._state = this._stack.pop();
  }

  // popNonInitial must be called by } ] and ) to pop non-initial states pushed by an opening bracket.
  popNonInitial() : void {
    while (this._state !== StateInitial && this._stack.length > 0) {
      this.pop();
    }
  }

{{end}}
