pro export_image, path

    idTool = itgetcurrent(tool=oTool)
    void = oTool->DoSetProperty('Operations/File/Export', 'SHOW_EXECUTION_UI', 0)
    void = oTool->DoSetProperty('Operations/File/Export', 'SOURCE', 1)
    void = oTool->DoSetProperty('Operations/File/Export', 'FILENAME', path)
    void = oTool->DoAction('Operations/File/Export')

end