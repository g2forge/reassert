package com.g2forge.reassert.express.model.variable;

import com.g2forge.alexandria.java.adt.name.INamed;
import com.g2forge.reassert.express.model.IExpression;

public interface IVariable<Name, Value> extends IExpression<Name, Value>, INamed<Name> {}
