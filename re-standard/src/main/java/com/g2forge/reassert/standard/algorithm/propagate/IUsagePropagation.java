package com.g2forge.reassert.standard.algorithm.propagate;

import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.contract.usage.IUsageApplied;

public interface IUsagePropagation extends IFunction2<IEdge, IUsageApplied, IUsageApplied> {}
