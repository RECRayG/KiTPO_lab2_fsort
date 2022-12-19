package com.lab2.java.doublyLinkedList;

public interface DoublyLinkedList<T> {
    void addM(T data);
    T getM(int index);
    void removeM(int index);
    void removeAllM();
    int sizeM();
    void addM(T data, int index);
    void forEachM(DoWith<T> a);
    void sort(Comparator<T> comparator);
}
