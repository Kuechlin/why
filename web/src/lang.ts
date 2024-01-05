import { CompletionContext, CompletionResult } from "@codemirror/autocomplete";
import {
    LRLanguage,
    LanguageSupport,
    delimitedIndent,
    foldInside,
    foldNodeProp,
    indentNodeProp,
} from "@codemirror/language";
import { Diagnostic, linter } from "@codemirror/lint";
import { styleTags, tags as t } from "@lezer/highlight";
import { WhyErr, why } from "ylang";
import { parser } from "./y.grammar";

const YLang = LRLanguage.define({
    name: "why",
    parser: parser.configure({
        props: [
            indentNodeProp.add({
                Application: delimitedIndent({ closing: ")", align: false }),
            }),
            foldNodeProp.add({
                Application: foldInside,
            }),
            styleTags({
                Identifier: t.variableName,
                Number: t.number,
                Boolean: t.bool,
                String: t.string,
                If: t.keyword,
                Let: t.keyword,
                Block: t.brace,
                Fn: t.function(t.keyword),
                Type: t.typeName,
                "( )": t.paren,
            }),
        ],
    }),
});

export function YLanguage() {
    return new LanguageSupport(YLang, [
        YLangLint,
        YLang.data.of({ autocomplete: ylangCompletions }),
    ]);
}

export const YLangLint = linter((view) => {
    const diagnostics: Diagnostic[] = [];
    try {
        why(view.state.doc.toString());
    } catch (err) {
        if (err instanceof WhyErr) {
            diagnostics.push({
                from: err.start,
                to: err.end,
                message: err.message,
                severity: "error",
            });
        }
    }
    return diagnostics;
});

const keywords = ["let", "fn", "number", "string", "boolean"];

function ylangCompletions(context: CompletionContext): CompletionResult | null {
    let check = context.matchBefore(/: ?/) ?? context.matchBefore(/\-> ?/);
    if (check) {
        return {
            from: context.pos,
            options: [
                {
                    label: "number",
                    type: "type",
                },
                {
                    label: "string",
                    type: "type",
                },
                {
                    label: "boolean",
                    type: "type",
                },
            ],
        };
    }
    const word = context.matchBefore(/\w*/);
    if (!word || (word.from == word.to && !context.explicit)) return null;
    if (keywords.some((k) => k.startsWith(word.text))) {
        return {
            from: word.from,
            options: keywords.map((label) => ({ label, type: "keyword" })),
        };
    }
    return {
        from: word.from,
        options: [
            {
                label: "let",
                type: "keyword",
            },
            {
                label: "if",
                type: "keyword",
            },
            {
                label: "fn",
                type: "keyword",
            },
        ],
    };
}