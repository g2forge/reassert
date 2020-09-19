package com.g2forge.reassert.contract.convert;

import com.g2forge.enigma.backend.convert.IExplicitRenderable;
import com.g2forge.reassert.contract.convert.IReportRenderContext;
import com.g2forge.reassert.express.explain.model.IExplained;

public interface IExplicitReportRenderable<T> extends IExplicitRenderable<IReportRenderContext>, IExplained<T> {}
