package com.lab2.java;

import com.lab2.java.doublyLinkedList.LinkedList;
import com.lab2.java.doublyLinkedList.ObjectBuilderFactory;
import com.lab2.java.doublyLinkedList.build.ObjectBuilder;
import com.lab2.java.gui.GUI;
import com.lab2.java.gui.ListActionListener;
import com.lab2.java.gui.ListActionListenerOverride;

public class Main {
    private static void testIt(ObjectBuilder builder) {
        for (int i = 1; i < 2000; i *= 2) {
            int n = i * 1000;
            System.out.println("N = " + n);
            LinkedList<Object> list = new LinkedList<>();
            for (int j = 0; j < n; j++) list.addM(builder.create());

            long start = System.nanoTime();

            try {
                list.sort(builder.getTypeComparator());
            } catch (StackOverflowError ignored) {
                System.err.println("Ошибка сортировки!");
                return;
            }
            long end = System.nanoTime();
            System.out.println("Время сортировки: " + (end - start) * 1.0 / 1000000 + " мс");
        }
    }

    public static void main(String[] args) {
        ObjectBuilder builder = ObjectBuilderFactory.getBuilderByName("String");

        testIt(builder);

        LinkedList<Object> list = new LinkedList<>();
        list.addM(builder.create());
        list.addM(builder.create());
        list.addM(builder.create());
        list.addM(builder.create());
        list.addM(builder.create());
        System.out.println("\nИнициализация двусвязного разомкнутого списка");
        list.forEachM(System.out::println);

        list.sort(builder.getTypeComparator());
        System.out.println("\nСортировка");
        list.forEachM(System.out::println);

        list.removeM(1);
        System.out.println("\nУдаление элемента с индексом 1");
        list.forEachM(System.out::println);

        list.removeM(0);
        System.out.println("\nУдаление элемента с индексом 0");
        list.forEachM(System.out::println);

        list.removeM(2);
        System.out.println("\nУдаление элемента с индексом 2");
        list.forEachM(System.out::println);

        list.addM(builder.create(), 1);
        System.out.println("\nДобавление элемента с индексом 1");
        list.forEachM(System.out::println);

        list.addM(builder.create(), 0);
        System.out.println("\nДобавление элемента с индексом 0");
        list.forEachM(System.out::println);

        list.addM(builder.create(), 2);
        System.out.println("\nДобавление элемента с индексом 2");
        list.forEachM(System.out::println);

        ListActionListener listActionListener = new ListActionListenerOverride();
        new GUI(listActionListener);
    }
}
