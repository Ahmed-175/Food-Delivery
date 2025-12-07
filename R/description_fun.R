description_fun_distance <- function(distance) {
    if (distance <= 8) {
        return("Short distance")
    } else {
        return("Long distance")
    }
}

description_fun_experience <- function(experience) {
    if (experience <= 4.5) {
        return("Little experience")
    } else {
        return("Extensive experience")
    }
}

description_fun_delivery_time <- function(delivery_time) {
    if (delivery_time <= 40) {
        return("Quick Deliveries")
    } else {
        return("Slow Deliveries ")
    }
}
