
int main(){
        //TEST GE
        if(3>=3){
            print("GE_OK");
        }else{
            print("GE_FAIL");
        }
        if(4>=3){
            print("GE_OK");
        }else{
            print("GE_FAIL");
        }
        if(2>=3){
            print("GE_FAIL");
        }else{
            print("GE_OK");
        }
        
        //TEST G
        if(5>3){
            print("G_OK");
        }else{
            print("G_FAIL");
        }
        if(3>3){
            print("G_FAIL");
        }else{
            print("G_OK");
        }
        if(-2> -1){
            print("G_FAIL");
        }else{
            print("G_OK");
        }
        
        //TEST EQ
        if(5==5){
            print("EQ_OK");
        }else{
            print("EQ_FAIL");
        }
        if(-3== -3){             
            print("EQ_OK");
        }else{
            print("EQ_FAIL");
        }
        if(-2==3){
            print("EQ_FAIL");
        }else{
            print("EQ_OK");
        }
        if(2== -3){              
            print("EQ_FAIL");
        }else{
            print("EQ_OK");
        }
        
        
        //TEST L
        if(5<6){
            print("L_OK");
        }else{
            print("L_FAIL");
        }
        if(-3< -2){             
            print("L_OK");
        }else{
            print("L_FAIL");
        }
        if(-2<3){
            print("L_OK");
        }else{
            print("L_FAIL");
        }
        if(2< -3){              
            print("L_FAIL");
        }else{
            print("L_OK");
        }
        if(-2< -3){              
            print("L_FAIL");
        }else{
            print("L_OK");
        }
        
        //TEST LE
        if(5<=6){
            print("LE_OK");
        }else{
            print("LE_FAIL");
        }
        if(5<=5){             
            print("LE_OK");
        }else{
            print("LE_FAIL");
        }
        if(5<=4){
            print("LE_FAIL");
        }else{
            print("LE_OK");
        }
        if(5<= -2){              
            print("LE_FAIL");
        }else{
            print("LE_OK");
        }
        if(-2<= -3){              
            print("LE_FAIL");
        }else{
            print("LE_OK");
        }
        
        
        //TEST NE
        if(5!=6){
            print("NE_OK");
        }else{
            print("NE_FAIL");
        }
        if(5!=5){          
            print("NE_FAIL");   
        }else{
            print("NE_OK");
        }
        if(5!= -5){
            print("NE_OK");
        }else{
            print("NE_FAIL");
        }
        
        //string compare
        if("ahoj"=="ahoj"){
            print("str EQ ok");
        } else {
            print("str EQ fail");
        }
        if("ahoj">="ahoj"){
            print("str GE ok");
        } else {
            print("str GE fail");
        }
        if("ahoj"<="ahoj"){
            print("str LE ok");
        } else {
            print("str LE fail");
        }
        
        if("ahoj"<="ahojj"){
            print("str LE ok");
        } else {
            print("str LE fail");
        }
        if("ahoj"<"ahojj"){
            print("str L ok");
        } else {
            print("str L fail");
        }
        if("ahoj">"ahojj"){
            print("str L fail");
        } else {
            print("str L ok");
        }
        
        
        if("ahoi"<="ahoj"){
            print("str LE ok");
        } else {
            print("str LE fail");
        }
        if("ahoi"<"ahojj"){
            print("str L ok");
        } else {
            print("str L fail");
        }
        if("ahoi">"ahoj"){
            print("str G fail");
        } else {
            print("str G ok");
        }
}