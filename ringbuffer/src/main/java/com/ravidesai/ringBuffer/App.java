package com.ravidesai.ringBuffer;

public class App 
{
    public static void main( String[] args )
    {
        System.out.println( "Hello World!" );
        StringRingBuffer rb = new StringRingBuffer(4);
        System.out.println("size is " + rb.size());

        rb.push("foo");
        rb.push("bar");
        rb.print();
        System.out.println("pop=" + rb.pop());
        rb.print();
        rb.push("baz");
        rb.pop();
        rb.print();
        rb.push("qux");
        rb.push("quux");
        rb.push("foo2");
        rb.print();
        rb.pop(); rb.pop(); rb.pop();
        rb.print();
    }
}
