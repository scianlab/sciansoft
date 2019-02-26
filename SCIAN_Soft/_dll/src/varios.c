  if(argv[0]->flags & IDL_V_CONST)
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "argv[0] es IDL_V_CONST");
  if(argv[0]->flags & IDL_V_TEMP)
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "argv[0] es IDL_V_TEMP");
  if(argv[0]->type == IDL_TYP_INT)
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "argv[0] es IDL_TYP_INT");
  if(argv[0]->type == IDL_TYP_LONG)
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "argv[0] es IDL_TYP_LONG");
  if(argv[0]->type == IDL_TYP_FLOAT)
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "argv[0] es IDL_TYP_FLOAT");
  if(argv[0]->type == IDL_TYP_UNDEF)
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "argv[0] es IDL_TYP_UNDEF");
  if(argv[0]->type == IDL_TYP_UINT)
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "argv[0] es IDL_TYP_UINT");
  if(argv[0]->type == IDL_TYP_STRING)
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "argv[0] es IDL_TYP_STRING");
  if(argv[0]->type == IDL_TYP_PTR)
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "argv[0] es IDL_TYP_PTR");
  if(argv[0]->type == IDL_TYP_OBJREF)
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, "argv[0] es IDL_TYP_OBJREF");


/*
    sprintf(msg, "value->type: %d.", value->type);
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, msg);
*/
