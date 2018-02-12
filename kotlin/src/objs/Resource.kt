package objs

/**
 * Singleton for Kotlin
 */
object Resource {
    val name: String = "name"
    val age: Int = 6

    init {
        println("Singleton Resource loading...")
    }

    operator fun invoke() {
        println("Resource Info:[name = $name, age = $age]")
    }

}