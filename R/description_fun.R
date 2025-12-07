description_fun_distance <- function(distance,distance_mean) {
    if (distance <= distance_mean) {
        return("Short distance")
    } else {
        return("Long distance")
    }
}

description_fun_experience <- function(experience, experience_mean) {
    if (experience <= experience_mean) {
        return("Little experience")
    } else {
        return("Extensive experience")
    }
}

description_fun_delivery_time <- function(delivery_time,delivery_time_mean) {
    if (delivery_time <= delivery_time_mean) {
        return("Quick Deliveries")
    } else {
        return("Slow Deliveries ")
    }
}
