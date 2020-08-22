package com.g2forge.reassert.contract.eee.explain.convert;

import com.g2forge.enigma.backend.convert.IExplicitRenderable;
import com.g2forge.reassert.contract.eee.explain.model.IExplained;

public interface IExplicitExplanationRenderable<T> extends IExplicitRenderable<IExplanationRenderContext>, IExplained<T> {}
