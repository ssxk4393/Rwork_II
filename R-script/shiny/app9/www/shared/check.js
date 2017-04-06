function data_check(){
    if($('#id').val()==''){
        alert('ID를 입력하세요')
        $('#id').focus();
        return false;
    }
    if($('#pwd').val()==''){
        alert('패스워드를 입력하세요')
        $('#pwd').focus();
        return false;
    } 
}