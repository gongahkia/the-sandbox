import React, { useContext } from "react";
import { DistractionContext } from "../context/DistractionContext";

const Notification: React.FC = () => {
  const { showNotification, notificationMessage } = useContext(DistractionContext);

  if (!showNotification) return null;

  return (
    <div className="fixed top-4 right-4 bg-yellow-100 border-l-4 border-yellow-500 text-yellow-700 p-4 rounded shadow">
      {notificationMessage}
    </div>
  );
};

export default Notification;