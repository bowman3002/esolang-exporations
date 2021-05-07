stack-push-0   
read-num-to-heap-0	
		stack-push-1   	
read-num-to-heap-1	
		call-copy-heap
 	 
subtract-stack	  	swap-needed-jump
		 	  
jmp-to-main
 
 	 	
------------------------------copy-heap:label-0
   
stack-push-0    
call-get-heap
 		
stack-push-1   	
call-get-heap
 		
return
	
end------------------------------get-heap:label-1
  	
read-heap-at-stack-top			return
	
end-------------------------------overwrite-heap:label-2
   	 
stack-push-1   	
swap 
	store-on-heap		 stack-push-0    
swap 
	store-on-heap		 return
	
end:overwrite-heap---------------------swap-heap:label-3
   		
call-copy-heap
 	 
swap-stack 
	call-overwrite-heap
 	 	 
return
	
end:swap-heap--------------------------swap-then-main:label-4
   	  
call-swap-heap
 	 		
main:label-5
   	 	
call-copy-heap
 	 
modulo	 		dup 
 end-if-zero
	  		 
call-swap-heap
 	 		
push-1   	
swap 
	save-to-heap		 jmp-to-main
 
 	 	
-----------------------------------------end-program:label-6
   		 
push-1   	
read-heap-at-1				
 	



