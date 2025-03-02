import React, { useState, useEffect } from 'react';
import { View, Text, Button } from 'react-native';
import { Pedometer } from 'expo-sensors';
import { supabase } from '../lib/supabase';

export default function StepCounter({ userId }: { userId: string }) {
  const [stepCount, setStepCount] = useState(0);

  useEffect(() => {
    let subscription: any;

    const subscribe = async () => {
      const isAvailable = await Pedometer.isAvailableAsync();
      if (isAvailable) {
        subscription = Pedometer.watchStepCount(result => {
          setStepCount(result.steps);
        });
      }
    };

    subscribe();

    return () => {
      subscription && subscription.remove();
    };
  }, []);

  const saveSteps = async () => {
    const { data, error } = await supabase
      .from('steps')
      .upsert({ user_id: userId, date: new Date().toISOString().split('T')[0], count: stepCount })
      .select();

    if (error) {
      console.error('Error saving steps', error);
    } else {
      console.log('Steps saved', data);
    }
  };

  return (
    <View>
      <Text>Steps taken today: {stepCount}</Text>
      <Button title="Save Steps" onPress={saveSteps} />
    </View>
  );
}