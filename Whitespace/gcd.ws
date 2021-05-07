stack-push-0   
read-num-to-heap-0	
		stack-pop 

stack-push-1   	
read-num-to-heap-1	
		stack-pop 

call-copy-heap
 	 
subtract-stack	  	swap-needed-jump
			  
copy-heap:label-0
   
stack-push-0    
call-get-heap
 		
stack-push-1   	
call-get-heap
 		
return
	
end----get-heap:label-1
  	
read-heap-at-stack-top			swap-stack 
	pop-stack-top 

return
	
end----overwrite-heap:label-2
   	 
stack-push-0    
store-on-heap		 stack-pop 

stack-pop 

stack-push-1   	
store-on-heap		 stack-pop 

stack-pop 

return
	
end:overwrite-heap-----swap-heap:label-3
   		
call-copy-heap
 	 
swap-stack 
	call-overwrite-heap
 	 	 
return
	
end:swap-heap-----swap-then-main:label-4
   	  
   
