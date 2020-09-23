package com.g2forge.reassert.contract.convert;

import com.g2forge.enigma.backend.convert.IExplicitRenderable;
import com.g2forge.reassert.express.model.IExplained;

public interface IExplicitReportRenderable<Value> extends IExplicitRenderable<IReportRenderContext>, IExplained<Value> {}
