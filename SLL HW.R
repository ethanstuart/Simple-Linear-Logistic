views <- read.table(header=TRUE,text="
State Viewed Temples
Alabama 0 1 
Alaska 0 1
Arizona 1 6
Arkansas 0 0
California 1 7
Colorado 1 2
Connecticut 0 1  
Delaware 0 0
Florida 1 2
Georgia 1 1
Hawaii 0 0
Idaho 1 5
Illinois 1 2
Indiana 1 1
Iowa 0 0
Kansas 0 0
Kentucky 0 1
Louisiana 0 1
Maine 0 0
Maryland 0 1
Massachusetts 0 1
Michigan 0 1
Minnesota 0 0
Mississippi 0 0
Missouri 1 0
Montana 0 1
Nebraska 0 0
Nevada 1 2
NewHampshire 0 0
NewJersey 0 0
NewMexico 1 1
NewYork 1 2
NorthCarolina 1 1
NorthDakota 0 0
Ohio 1 1
Oklahoma 0 1
Oregon 1 2
Pennsylvania 1 1
RhodeIsland 0 0
SouthCarolina 0 1
SouthDakota 0 0
Tennessee 1 2
Texas 1 4
Utah 1 17
Vermont 0 0
Virginia 1 0
Washington 1 3
WestVirginia 0 0
Wisconsin 0 0
Wyoming 1 1"
)


#Model
# log(P(viewers>500|Temples)/P(viewers<500|Temples))= beta0 + beta1 GPA
out.views <- glm(Viewed~Temples,data=views, family="binomial")
summary(out.views)
confint(out.views)
