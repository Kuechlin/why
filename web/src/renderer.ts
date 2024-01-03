import { WhyErr } from "ylang";

function s(val: string, type: string) {
    return `<span class="${type}">${val}</span>`;
}

export function value_to_string(val: unknown): string {
    switch (typeof val) {
        case "string":
            return s(`"${val}"`, "string") + s(": string", "type");
        case "number":
        case "bigint":
            return s(val.toLocaleString(), "number") + s(": number", "type");
        case "boolean":
            return (
                s(val ? "true" : "false", "boolean") + s(": boolean", "type")
            );
        case "undefined":
            return s("void", "void");
        case "function":
        case "object":
        case "symbol":
            return s("invalid value", "error");
    }
}

export function error_to_string(err: unknown): string {
    if (!(err instanceof WhyErr)) return String(err);

    return `${err.message} at ${err.start}..${err.end}`;
}
