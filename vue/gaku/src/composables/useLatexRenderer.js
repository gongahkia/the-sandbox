import { computed } from 'vue';
import katex from 'katex';

export function useLatexRenderer(content) {
  const renderedLatex = computed(() => {
    try {
      let rendered = content.value;
      
      // Replace $$ ... $$ with rendered LaTeX (display mode)
      const mathRegex = /\$\$(.*?)\$\$/gs;
      rendered = rendered.replace(mathRegex, (match, formula) => {
        try {
          return katex.renderToString(formula, { 
            displayMode: true,
            throwOnError: false
          });
        } catch (e) {
          return `<div class="error">LaTeX Error: ${e.message}</div>`;
        }
      });
      
      // Replace $ ... $ with inline LaTeX
      const inlineMathRegex = /\$(.*?)\$/g;
      rendered = rendered.replace(inlineMathRegex, (match, formula) => {
        try {
          return katex.renderToString(formula, { 
            displayMode: false,
            throwOnError: false
          });
        } catch (e) {
          return `<span class="error">LaTeX Error: ${e.message}</span>`;
        }
      });
      
      return rendered;
    } catch (e) {
      return `<div class="error">Rendering Error: ${e.message}</div>`;
    }
  });

  return {
    renderedLatex
  };
}