package com.ravidesai.ringBuffer;

import java.util.Arrays;

public class StringRingBuffer {
    private int size;
    private String[] array;
    int tail = 0;
    int head = 0;
    StringRingBuffer(int consize) {
        size = consize;
        array = new String[size];
    }

    int size() {
        return size;
    }

    void push(String s1) {
        array[tail] = s1;

        if (tail == size-1) {
            tail = 0;
        } else {
            tail++;
        }
    }

    void print() {
        System.out.println(Arrays.toString(array));
    }

    String pop() {
        var result = array[head];
        array[head] = null;

        if (head == size -1) {
            head = 0;
        } else {
            head++;
        }

        return result;
    }
}
