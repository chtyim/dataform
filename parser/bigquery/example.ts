import { createTreeFromSqlStatement } from './ast/parse';
import { Node } from './ast/tree';
import { NodeFlags, NodeType } from './listener';
import { Parser, StopOnFirstError } from './parser';
import { Any } from './selector';
import { TokenStream } from './stream';

let src = `
WITH Prep1 AS (
    FROM \`Patient Medication\`
    -- This expression converts the integer value representing seconds since epoch to a timestamp.
    |> SET VisitDate = TIMESTAMP_SECONDS(VisitDate)
    -- Converts column Birthdate from type string to date with the following format(s): '%Y-%m-%d','%m/%d/%Y'
    |> SET BirthDate = COALESCE(SAFE.PARSE_DATE('%Y-%m-%d', Birthdate),SAFE.PARSE_DATE('%m/%d/%Y', Birthdate))
    -- Transforms height from feet to centimeters and add cm to the end of the expression.
    |> EXTEND COALESCE(ROUND(CAST(REGEXP_EXTRACT(Height, r'^\\s*(-?[0-9eE.]+)\\s*ft\$') AS NUMERIC) * CAST(30.48 AS NUMERIC), 2) || ' cm', Height) AS Height_cm
    -- Remove the Height column
    |> DROP Height
    -- This expression extracts the value of the "Notes" key from a JSON
    |> SET Notes = REGEXP_REPLACE(Notes, r'^{\\"Notes\\":\\"(.*)\\"}\$', r'\\1')
    -- Converts column Age from type string to int64
    |> SET Age = SAFE_CAST(Age AS int64)
    |> /* @@VALIDATE */ WHERE age >= 0
), Join1 AS (
    FROM Prep1
    |> JOIN \`HospitalLookup\` AS r ON \`Hospital ID\` = r.ID
    |> DROP ID
    |> RENAME Name AS HospitalName
), Prep2 AS (
    FROM Join1
    -- This expression removes any parenthesized number from the end of the string.
    |> SET Hospital = REGEXP_REPLACE(Hospital, r' \\(\\d+\\)', r'')
), Join2 AS (
    FROM Prep2
    |> LEFT JOIN \`PatientCharges\` AS r ON Hospital = r.\`Provider Name\`
)
FROM Join2
|> LIMIT 20
`;

// let src = `SELECT * FROM \`Patient Medication\``;
// let src = "SELECT 1";

let logger = (t: NodeType, flags: NodeFlags, offset: number, endOffset: number) => {
  console.log("type: " + NodeType[t] + ", flags: " + flags + ", offset: " + offset + ", end: " + endOffset);
};

// Event based parser
let p = new Parser(StopOnFirstError, logger);
let tstream = new TokenStream(src, logger);
let res = p.parseSqlStatement(tstream);

if (res.err !== null && res.err !== undefined) {
  throw new Error("Error occurred at line " + res.err.line + ": '" + src.substring(res.err.offset, res.err.endoffset) + "'");
}

// AST
let { tree, err } = createTreeFromSqlStatement(src, StopOnFirstError);
if (tree) {
  traverse(tree.root(), "");
} else if (err) {
  throw err;
}

function traverse(node: Node, indent: string) {
  let log = indent + NodeType[node.type()] + "(" + node.offset() + ":" + node.endOffset() + ")";
  if (node.child(Any) === null) {
    log = log + "\t" + node.text();
  }
  console.log(log);
  node.children(Any).forEach(n => traverse(n, indent + "  "));
}
