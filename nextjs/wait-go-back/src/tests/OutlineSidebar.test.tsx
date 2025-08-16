import { render, fireEvent } from '@testing-library/react'
import OutlineSidebar from '../components/OutlineSidebar'

test('renders outline and handles navigation', () => {
  const outline = [
    { id: 'h1', text: 'Heading 1', level: 1, position: 0 },
    { id: 'h2', text: 'Heading 2', level: 2, position: 1 },
  ]
  const onNavigate = jest.fn()
  const onRename = jest.fn()
  const { getByDisplayValue } = render(
    <OutlineSidebar outline={outline} onNavigate={onNavigate} onRename={onRename} />
  )
  fireEvent.click(getByDisplayValue('Heading 1'))
  expect(onNavigate).toHaveBeenCalledWith(0)
})