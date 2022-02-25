
class Light():

    def __init__(self,angle=90.0,direction=180.0):
        self.__light_angle = angle
        self.__light_direction = direction
    
    def angle(self):
        return self.__light_angle 

    def direction(self):
        return self.__light_direction 
