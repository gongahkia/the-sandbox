import React, { useEffect, useState } from 'react';
import { fetchDirectory, FileNode } from '../filesystem/dirApi';
import { FileIcon } from '../icons/FileIcon';
import { useExplorerActions } from '../actions/useExplorerActions';

export default function ExplorerView({ rootPath = '/' }) {
  const [tree, setTree] = useState<FileNode[]>([]);
  const { openFile } = useExplorerActions();

  useEffect(() => {
    fetchDirectory(rootPath).then(setTree);
  }, [rootPath]);

  return (
    <ul className="file-tree">
      {tree.map(entry => (
        <li key={entry.path} onClick={() => entry.isFile && openFile(entry.path)}>
          <FileIcon type={entry.isFile ? entry.ext : 'folder'} />
          {entry.name}
        </li>
      ))}
    </ul>
  );
}
import React, { useEffect, useState } from 'react';
import { fetchDirectory, FileNode } from '../filesystem/dirApi';
import { FileIcon } from '../icons/FileIcon';
import { useExplorerActions } from '../actions/useExplorerActions';

export default function ExplorerView({ rootPath = '/' }) {
  const [tree, setTree] = useState<FileNode[]>([]);
  const { openFile } = useExplorerActions();

  useEffect(() => {
    fetchDirectory(rootPath).then(setTree);
  }, [rootPath]);

  return (
    <ul className="file-tree">
      {tree.map(entry => (
        <li key={entry.path} onClick={() => entry.isFile && openFile(entry.path)}>
          <FileIcon type={entry.isFile ? entry.ext : 'folder'} />
          {entry.name}
        </li>
      ))}
    </ul>
  );
}