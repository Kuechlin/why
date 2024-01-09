import van from "vanjs-core";

const { div, br, article, span } = van.tags;

const s = (
    key:
        | "variable"
        | "number"
        | "string"
        | "boolean"
        | "type"
        | "keyword"
        | "function"
        | "space"
        | "comment",
    val: string
) => span({ class: key }, val);

export function Docs() {
    return div(
        { class: "docs" },
        article(
            div({ class: "title" }, "variables"),
            div(s("comment", "- immutable\n- must be initialized")),
            div(s("variable", "a"), span(" = "), s("number", "5")),
            div(s("variable", "b"), span(" = "), s("string", '"hello"')),
            div(s("variable", "c"), span(" = "), s("boolean", "true"))
        ),
        article(
            div({ class: "title" }, "compare"),
            div(s("comment", "compare values")),
            div(s("number", "4"), span(" < "), s("number", "5")),
            div(s("number", "1"), span(" <= "), s("number", "3")),
            div(s("number", "10"), span(" > "), s("number", "2")),
            div(s("number", "8"), span(" >= "), s("number", "5")),
            div(s("comment", "check equality")),
            div(s("string", '"a"'), span(" == "), s("string", '"a"')),
            div(s("boolean", "true"), span(" != "), s("boolean", "false"))
        ),
        article(
            div({ class: "title" }, "math"),
            div(s("comment", "add")),
            div(
                s("number", "8"),
                span(" + "),
                s("number", "8"),
                s("comment", " = 16")
            ),
            div(s("comment", "substract")),
            div(
                s("number", "16"),
                span(" - "),
                s("number", "8"),
                s("comment", " = 8")
            ),
            div(s("comment", "multiply")),
            div(
                s("number", "4"),
                span(" * "),
                s("number", "4"),
                s("comment", " = 16")
            ),
            div(s("comment", "divide")),
            div(
                s("number", "8"),
                span(" / "),
                s("number", "2"),
                s("comment", " = 4")
            )
        ),
        article(
            div({ class: "title" }, "concat"),
            div(s("comment", "strings can be combined\nwith any value")),
            div(s("string", '"hello "'), span(" + "), s("string", '"world"')),
            div(s("string", '"val: "'), span(" + "), s("number", "8")),
            div(s("string", '"is "'), span(" + "), s("boolean", "true")),
            div(s("string", '"hello "'), span(" + "), s("variable", "name"))
        ),
        article(
            div({ class: "title" }, "functions"),
            div(s("comment", "- functions are values")),
            div(s("comment", "- functions are pure")),
            div(
                s("variable", "add"),
                span(" = "),
                s("keyword", "fn "),
                s("variable", "a"),
                span(": "),
                s("type", "Num"),
                span(", "),
                s("variable", "b"),
                span(": "),
                s("type", "Num"),
                s("keyword", " -> "),
                s("type", "Num"),
                span(" {")
            ),
            div(
                s("space", "    "),
                s("variable", "a"),
                span(" + "),
                s("variable", "a")
            ),
            div(span("};")),
            br(),
            div(
                s("function", "add"),
                span("("),
                s("number", "4"),
                span(", "),
                s("number", "8"),
                span(")")
            )
        ),
        article(
            div({ class: "title" }, "functions in functions"),
            div(s("comment", "kotlin style lambdas")),
            div(s("comment", "args have same name as in type definition")),
            div(
                s("variable", "wrapper"),
                span(" = "),
                s("keyword", "fn "),
                s("variable", "inner"),
                span(": ("),
                s("keyword", "fn "),
                s("variable", "a"),
                span(": "),
                s("type", "Num"),
                s("keyword", " -> "),
                s("type", "Num"),
                span(")"),
                s("keyword", " -> "),
                s("type", "Num"),
                span(" {")
            ),
            div(
                s("space", "    "),
                s("function", "inner"),
                span("("),
                s("number", "16"),
                span(")")
            ),
            div(span("};")),
            br(),
            div(
                s("function", "wrapper"),
                span("({ "),
                s("variable", "a"),
                span(" * "),
                s("number", "8"),
                span("})")
            )
        ),
        article(
            div({ class: "title" }, "match"),
            div(s("comment", "use match for union types")),
            div(s("variable", "some"), s("keyword", " is {")),
            div(
                s("space", "    "),
                s("keyword", ": "),
                s("type", "Num"),
                s("keyword", " -> it"),
                span(" + "),
                s("number", "5"),
                span(",")
            ),
            div(
                s("space", "    "),
                s("keyword", ": "),
                s("type", "Str"),
                s("keyword", " -> "),
                s("string", '"val: "'),
                span(" + "),
                s("keyword", "it"),
                span(",")
            ),
            div(
                s("space", "    "),
                s("keyword", "-> "),
                s("string", '"default"'),
                span(",")
            ),
            div(s("keyword", "}")),
            div(s("comment", "use match for multiple compares")),
            div(s("variable", "some"), s("keyword", " is {")),
            div(
                s("space", "    "),
                s("keyword", "> "),
                s("number", "5"),
                s("keyword", " -> it"),
                span(" * "),
                s("number", "5"),
                span(",")
            ),
            div(
                s("space", "      "),
                s("number", "5"),
                s("keyword", " -> it"),
                span(" + "),
                s("number", "5"),
                span(",")
            ),
            div(
                s("space", "    "),
                s("keyword", "< "),
                s("number", "5"),
                s("keyword", " -> it"),
                span(" / "),
                s("number", "5"),
                span(",")
            ),
            div(s("keyword", "}"))
        ),
        article(
            div({ class: "title" }, "types"),
            div(s("comment", "define custom types")),
            div(
                s("keyword", "def "),
                s("type", "Union"),
                span(" : ("),
                s("type", "Str"),
                span(" | "),
                s("type", "Num"),
                span(");")
            ),
            div(
                s("keyword", "def "),
                s("type", "SomeFunction"),
                span(" : ("),
                s("keyword", "fn "),
                s("variable", "a"),
                span(": "),
                s("type", "Num"),
                s("keyword", " -> "),
                s("type", "Num"),
                span(");")
            )
        )
    );
}
