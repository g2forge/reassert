package com.g2forge.reassert.express.v2.convert;

import com.g2forge.enigma.backend.convert.IExplicitRenderable;
import com.g2forge.reassert.express.v2.model.IExplained;

public interface IExplicitExplanationRenderable<Name, Value> extends IExplicitRenderable<IExplanationRenderContext<Name, Value>>, IExplained<Value> {}
