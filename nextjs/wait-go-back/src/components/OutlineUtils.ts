export type OutlineEntry = {
  id: string
  text: string
  level: number
  position: number
}

export function extractOutline(doc: any): OutlineEntry[] {
  const outline: OutlineEntry[] = []
  let pos = 0

  function traverse(node: any, parentLevel = 1) {
    if (!node) return
    if (node.type === 'heading') {
      outline.push({
        id: node.attrs?.id || `${node.type}-${pos}`,
        text: node.content?.[0]?.text || 'Untitled',
        level: node.attrs?.level || 1,
        position: pos,
      })
    }
    if (node.content) {
      node.content.forEach((child: any) => {
        pos += 1
        traverse(child, node.attrs?.level || parentLevel)
      })
    }
  }

  traverse(doc)
  return outline
}