import React, { useContext } from "react";
import { DistractionContext } from "../context/DistractionContext";

const DistractionCounter: React.FC = () => {
  const { distractionCount, pauseCount } = useContext(DistractionContext);

  return (
    <div className="flex items-center gap-4 text-gray-700 dark:text-gray-200">
      <span>Distractions: <b>{distractionCount}</b></span>
      <span>Pauses: <b>{pauseCount}</b></span>
    </div>
  );
};

export default DistractionCounter;