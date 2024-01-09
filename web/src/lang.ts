import { CompletionContext, CompletionResult } from "@codemirror/autocomplete";
import {
    LRLanguage,
    LanguageSupport,
    delimitedIndent,
    foldInside,
    foldNodeProp,
    indentNodeProp,
} from "@codemirror/language";
import { linter } from "@codemirror/lint";
import { styleTags, tags as t } from "@lezer/highlight";
import { analyse } from "ylang";
import { parser } from "./y.grammar";

const YLang = LRLanguage.define({
    name: "why",
    languageData: {
        closeBrackets: {},
        wordChars: ["if", "is", "let", "def", "fn"],
    },
    parser: parser.configure({
        props: [
            indentNodeProp.add({
                Application: delimitedIndent({ closing: ")", align: false }),
            }),
            foldNodeProp.add({
                Application: foldInside,
            }),
            styleTags({
                "Call/Identifier": t.function(t.variableName),
                Identifier: t.variableName,
                TypeIdentifier: t.typeName,
                Dot: t.typeName,
                Number: t.number,
                Boolean: t.atom,
                String: t.string,
                If: t.keyword,
                Is: t.keyword,
                Match: t.keyword,
                Def: t.keyword,
                Let: t.keyword,
                ObjInit: t.keyword,
                Block: t.brace,
                Fn: t.keyword,
                Type: t.typeName,
                Or: t.keyword,
                Func: t.keyword,
                Call: t.function(t.variableName),
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
    return analyse(view.state.doc.toString()).map((err) => ({
        from: err.start,
        to: err.end,
        message: err.message,
        severity: "error",
    }));
});

const keywords = ["let", "def", "fn", "Num", "Str", "Bool"];

function ylangCompletions(context: CompletionContext): CompletionResult | null {
    let check = context.matchBefore(/: ?/) ?? context.matchBefore(/\-> ?/);
    if (check) {
        return {
            from: context.pos,
            options: [
                {
                    label: "Num",
                    type: "type",
                },
                {
                    label: "Str",
                    type: "type",
                },
                {
                    label: "Bool",
                    type: "type",
                },
            ],
        };
    }
    const word = context.matchBefore(/\w*/);
    if (!word || (word.from == word.to && !context.explicit)) return null;

    if (word.text === "")
        return {
            from: word.from,
            options: [
                {
                    label: "let",
                    type: "keyword",
                },
                {
                    label: "def",
                    type: "keyword",
                },
            ],
        };

    if (keywords.some((k) => k.startsWith(word.text))) {
        return {
            from: word.from,
            options: keywords.map((label) => ({ label, type: "keyword" })),
        };
    }

    return null;
}
