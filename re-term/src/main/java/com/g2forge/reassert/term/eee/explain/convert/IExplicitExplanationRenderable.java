package com.g2forge.reassert.term.eee.explain.convert;

import com.g2forge.enigma.backend.convert.IExplicitRenderable;
import com.g2forge.reassert.term.eee.explain.model.IExplained;

public interface IExplicitExplanationRenderable<T> extends IExplicitRenderable<IExplanationRenderContext>, IExplained<T> {}
