import { extractOutline } from '../components/OutlineUtils'

test('extracts headings from document', () => {
  const doc = {
    type: 'doc',
    content: [
      { type: 'heading', attrs: { level: 1 }, content: [{ type: 'text', text: 'Title' }] },
      { type: 'paragraph', content: [{ type: 'text', text: 'Intro' }] },
      { type: 'heading', attrs: { level: 2 }, content: [{ type: 'text', text: 'Section' }] },
    ],
  }
  const outline = extractOutline(doc)
  expect(outline).toHaveLength(2)
  expect(outline[0].text).toBe('Title')
  expect(outline[1].level).toBe(2)
})