package com.g2forge.reassert.express.model.constant;

import com.g2forge.alexandria.java.adt.name.INamed;
import com.g2forge.alexandria.java.function.ISupplier;
import com.g2forge.reassert.express.model.IExplainedValue;

public interface ILiteral<Name, Value> extends IConstant<Name, Value>, INamed<Name>, ISupplier<Value>, IExplainedValue<Value> {}
