import markdownit from "markdown-it"
const md = markdownit();

// used in `Markdown.purs`
export function renderMarkdownToDiv(mdString) {
  return function(divId) {
    return function() {
      const div = document.getElementById(divId);
      div.innerHTML = md.render(mdString);
    }
  }
}

