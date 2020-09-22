package com.g2forge.reassert.contract.v2.convert;

import com.g2forge.enigma.backend.convert.IExplicitRenderable;
import com.g2forge.reassert.express.v2.model.IExplained;

public interface IExplicitReportRenderable<Value> extends IExplicitRenderable<IReportRenderContext>, IExplained<Value> {}
