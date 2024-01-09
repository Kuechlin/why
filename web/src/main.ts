import van from "vanjs-core";
import { About } from "./about";
import { Docs } from "./docs";
import { Editor } from "./editor";

const { div, nav } = van.tags;

const tabs = [
    { key: "#run", title: "Editor", content: Editor },
    { key: "#docs", title: "Documentation", content: Docs },
    { key: "#why", title: "Why", content: About },
];

function initial() {
    const i = tabs.findIndex((x) => x.key === location.hash);
    return i === -1 ? 0 : i;
}

function Layout() {
    const active = van.state(initial());
    function setActive(index: number) {
        return () => {
            history.pushState(null, "", tabs[index].key);
            active.val = index;
        };
    }
    return [
        () =>
            nav(
                tabs.map((x, i) =>
                    div(
                        {
                            onclick: setActive(i),
                            class: active.val === i ? "active" : "",
                        },
                        x.title
                    )
                )
            ),
        () => div({ class: "content" }, tabs[active.val].content()),
    ];
}

van.add(document.getElementById("content")!, Layout());
