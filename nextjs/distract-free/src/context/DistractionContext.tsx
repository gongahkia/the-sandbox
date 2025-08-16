import React, { createContext, useState } from "react";

interface DistractionContextProps {
  distractionCount: number;
  pauseCount: number;
  showNotification: boolean;
  notificationMessage: string;
  incrementDistraction: () => void;
  resetPause: () => void;
  setPauseDuration: (duration: number) => void;
}

export const DistractionContext = createContext<DistractionContextProps>(/* ... */);

export const DistractionProvider: React.FC = ({ children }) => {
  const [distractionCount, setDistractionCount] = useState(0);
  const [pauseCount, setPauseCount] = useState(0);
  const [showNotification, setShowNotification] = useState(false);
  const [notificationMessage, setNotificationMessage] = useState("");

  const incrementDistraction = () => {
    setDistractionCount((c) => c + 1);
    setShowNotification(true);
    setNotificationMessage("Stay focused! You switched away from the editor.");
    setTimeout(() => setShowNotification(false), 2000);
  };

  const resetPause = () => setPauseCount(0);

  const setPauseDuration = (duration: number) => {
    if (duration > 10) {
      setPauseCount((c) => c + 1);
      setShowNotification(true);
      setNotificationMessage("You paused for a while. Try to keep writing!");
      setTimeout(() => setShowNotification(false), 2000);
    }
  };

  return (
    <DistractionContext.Provider
      value={{
        distractionCount,
        pauseCount,
        showNotification,
        notificationMessage,
        incrementDistraction,
        resetPause,
        setPauseDuration,
      }}
    >
      {children}
    </DistractionContext.Provider>
  );
};