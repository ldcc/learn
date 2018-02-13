package objs

class Person(name: String) {
    init {
        println("Hello $name")
    }

    /**
     * 次级构造器
     */
    constructor(name: String, age: Int) : this(name) {
        println(age)
    }
}