package com.g2forge.reassert.expression.explain.convert;

import com.g2forge.enigma.backend.convert.IExplicitRenderable;
import com.g2forge.reassert.expression.explain.model.IExplained;

public interface IExplicitExplanationRenderable<T> extends IExplicitRenderable<IExplanationRenderContext>, IExplained<T> {}
