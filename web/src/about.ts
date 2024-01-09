import van from "vanjs-core";

const { div, h2 } = van.tags;

export function About() {
    return div({ class: "about" }, h2("some text about why it was created"));
}
